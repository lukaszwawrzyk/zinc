/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package sbt
package internal
package inc
package classfile

import scala.collection.mutable
import mutable.{ Buffer, ArrayBuffer }
import scala.annotation.tailrec
import java.io.File
import java.net.URL

import xsbti.api.DependencyContext
import xsbti.api.DependencyContext._
import sbt.io.IO.{ FileScheme, toFile }
import sbt.util.Logger

private[sbt] object Analyze {
  def apply[T](newClasses: Seq[File], sources: Seq[File], log: Logger)(
      analysis: xsbti.AnalysisCallback,
      loader: ClassLoader,
      readAPI: (File, Seq[Class[_]]) => Set[(String, String)]): Unit = {
    val sourceMap = sources.toSet[File].groupBy(_.getName)

    def load(tpe: String, errMsg: => Option[String]): Option[Class[_]] = {
      if (tpe.endsWith("module-info")) None
      else
        try {
          Some(Class.forName(tpe, false, loader))
        } catch {
          case e: Throwable => errMsg.foreach(msg => log.warn(msg + " : " + e.toString)); None
        }
    }

    val classNames = mutable.Set.empty[String]
    val sourceToClassFiles = mutable.HashMap[File, Buffer[ClassFile]](
      sources zip Seq.fill(sources.size)(new ArrayBuffer[ClassFile]): _*
    )

    val binaryClassNameToLoadedClass = new mutable.HashMap[String, Class[_]]

    val classfilesCache = mutable.Map.empty[String, File]

    // parse class files and assign classes to sources.  This must be done before dependencies, since the information comes
    // as class->class dependencies that must be mapped back to source->class dependencies using the source+class assignment
    for (newClass <- newClasses;
         classFile = Parser(newClass);
         _ <- classFile.sourceFile orElse guessSourceName(newClass.getName);
         source <- guessSourcePath(sourceMap, classFile, log);
         binaryClassName = classFile.className;
         loadedClass <- load(binaryClassName,
                             Some("Error reading API from class file: " + binaryClassName))) {
      binaryClassNameToLoadedClass.update(binaryClassName, loadedClass)

      def loadEnclosingClass(clazz: Class[_]): Option[String] = {
        binaryToSourceName(clazz) match {
          case None if clazz.getEnclosingClass != null =>
            loadEnclosingClass(clazz.getEnclosingClass)
          case other => other
        }
      }

      val srcClassName = loadEnclosingClass(loadedClass)

      srcClassName match {
        case Some(className) =>
          analysis.generatedNonLocalClass(source, newClass, binaryClassName, className)
          classNames += className
        case None => analysis.generatedLocalClass(source, newClass)
      }

      sourceToClassFiles(source) += classFile
    }

    // get class to class dependencies and map back to source to class dependencies
    for ((source, classFiles) <- sourceToClassFiles) {
      analysis.startSource(source)
      val loadedClasses = classFiles.map(c => binaryClassNameToLoadedClass(c.className))
      // Local classes are either local, anonymous or inner Java classes
      val (nonLocalClasses, localClassesOrStale) =
        loadedClasses.partition(_.getCanonicalName != null)

      // Map local classes to the sources of their enclosing classes
      val localClassesToSources = {
        val localToSourcesSeq = for {
          cls <- localClassesOrStale
          enclosingCls <- Option(cls.getEnclosingClass)
          sourceOfEnclosing <- binaryToSourceName(enclosingCls)
        } yield (cls.getName, sourceOfEnclosing)
        localToSourcesSeq.toMap
      }

      /* Get the mapped source file from a given class name. */
      def getMappedSource(className: String): Option[String] = {
        val nonLocalSourceName: Option[String] = for {
          loadedClass <- binaryClassNameToLoadedClass.get(className)
          sourceName <- binaryToSourceName(loadedClass)
        } yield sourceName
        nonLocalSourceName.orElse(localClassesToSources.get(className))
      }

      def processDependency(onBinaryName: String,
                            context: DependencyContext,
                            fromBinaryName: String): Unit = {
        def loadFromClassloader(): Option[File] = {
          for {
            url <- Option(loader.getResource(classNameToClassFile(onBinaryName)))
            file <- urlAsFile(url, log)
          } yield { classfilesCache(onBinaryName) = file; file }
        }

        getMappedSource(fromBinaryName) match {
          case Some(fromClassName) =>
            trapAndLog(log) {
              val scalaLikeTypeName = onBinaryName.replace('$', '.')
              if (classNames.contains(scalaLikeTypeName)) {
                analysis.classDependency(scalaLikeTypeName, fromClassName, context)
              } else {
                val cachedOrigin = classfilesCache.get(onBinaryName)
                for (file <- cachedOrigin.orElse(loadFromClassloader()))
                  analysis.binaryDependency(file, onBinaryName, fromClassName, source, context)
              }
            }
          case None => // It could be a stale class file, ignore
        }
      }
      def processDependencies(binaryClassNames: Iterable[String],
                              context: DependencyContext,
                              fromBinaryClassName: String): Unit =
        binaryClassNames.foreach(binaryClassName =>
          processDependency(binaryClassName, context, fromBinaryClassName))

      // Get all references to types in a given class file (via constant pool)
      val typesInSource = classFiles.map(cf => cf.className -> cf.types).toMap

      // Process dependencies by member references
      typesInSource foreach {
        case (binaryClassName, binaryClassNameDeps) =>
          processDependencies(binaryClassNameDeps, DependencyByMemberRef, binaryClassName)
      }

      def readInheritanceDependencies(classes: Seq[Class[_]]) = {
        val api = readAPI(source, classes)
        api.groupBy(_._1).mapValues(_.map(_._2))
      }

      // Read API of non-local classes and process dependencies by inheritance
      val nonLocalInherited: Map[String, Set[String]] =
        readInheritanceDependencies(nonLocalClasses)
      nonLocalInherited foreach {
        case (className, inheritanceDeps) =>
          processDependencies(inheritanceDeps, DependencyByInheritance, className)
      }

      // Read API of local classes and process local dependencies by inheritance
      val localClasses =
        localClassesOrStale.filter(cls => localClassesToSources.contains(cls.getName))
      val localInherited: Map[String, Set[String]] =
        readInheritanceDependencies(localClasses)
      localInherited foreach {
        case (className, inheritanceDeps) =>
          processDependencies(inheritanceDeps, LocalDependencyByInheritance, className)
      }
    }
  }
  private[this] def urlAsFile(url: URL, log: Logger): Option[File] =
    try {
      urlAsFile0(url)
    } catch {
      case e: Exception =>
        log.warn("Could not convert URL '" + url.toExternalForm + "' to File: " + e.toString)
        None
    }

  // copied and edited from IO
  private def urlAsFile0(url: URL): Option[File] =
    url.getProtocol match {
      case FileScheme => Some(toFile(url))
      case "jar" =>
        Some(new File(url.toString))
      case _ => None
    }

  private def trapAndLog(log: Logger)(execute: => Unit): Unit = {
    try { execute } catch { case e: Throwable => log.trace(e); log.error(e.toString) }
  }
  private def guessSourceName(name: String) = Some(takeToDollar(trimClassExt(name)))
  private def takeToDollar(name: String) = {
    val dollar = name.indexOf('$')
    if (dollar < 0) name else name.substring(0, dollar)
  }
  private final val ClassExt = ".class"
  private def trimClassExt(name: String) =
    if (name.endsWith(ClassExt)) name.substring(0, name.length - ClassExt.length) else name
  private def classNameToClassFile(name: String) = name.replace('.', '/') + ClassExt
  private def binaryToSourceName(loadedClass: Class[_]): Option[String] =
    Option(loadedClass.getCanonicalName)
  private def guessSourcePath(sourceNameMap: Map[String, Set[File]],
                              classFile: ClassFile,
                              log: Logger) = {
    val classNameParts = classFile.className.split("""\.""")
    val pkg = classNameParts.init
    val simpleClassName = classNameParts.last
    val sourceFileName =
      classFile.sourceFile.getOrElse(simpleClassName.takeWhile(_ != '$').mkString("", "", ".java"))
    val candidates = findSource(sourceNameMap, pkg.toList, sourceFileName)
    candidates match {
      case Nil      => log.warn("Could not determine source for class " + classFile.className)
      case _ :: Nil => ()
      case _ =>
        log.warn(
          "Multiple sources matched for class " + classFile.className + ": " + candidates.mkString(
            ", "))
    }
    candidates
  }
  private def findSource(sourceNameMap: Map[String, Iterable[File]],
                         pkg: List[String],
                         sourceFileName: String): List[File] =
    refine((sourceNameMap get sourceFileName).toList.flatten.map { x =>
      (x, x.getParentFile)
    }, pkg.reverse)

  @tailrec private def refine(sources: List[(File, File)], pkgRev: List[String]): List[File] = {
    def make = sources.map(_._1)
    if (sources.isEmpty || sources.tail.isEmpty)
      make
    else
      pkgRev match {
        case Nil => shortest(make)
        case x :: xs =>
          val retain = sources flatMap {
            case (src, pre) =>
              if (pre != null && pre.getName == x)
                (src, pre.getParentFile) :: Nil
              else
                Nil
          }
          refine(retain, xs)
      }
  }
  private def shortest(files: List[File]): List[File] =
    if (files.isEmpty) files
    else {
      val fs = files.groupBy(distanceToRoot(0))
      fs(fs.keys.min)
    }

  private def distanceToRoot(acc: Int)(file: File): Int =
    if (file == null) acc else distanceToRoot(acc + 1)(file.getParentFile)
}
