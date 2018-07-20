/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package sbt
package internal
package inc

import sbt.io.IO
import java.io.File
import java.net.URI
import java.nio.file._
import java.util.{ Optional, UUID }

import collection.mutable
import xsbti.compile.{
  ClassFileManagerType,
  DeleteImmediatelyManagerType,
  IncOptions,
  TransactionalManagerType,
  ClassFileManager => XClassFileManager
}

object ClassFileManager {
  def getDefaultClassFileManager(
      classFileManagerType: Optional[ClassFileManagerType]): XClassFileManager = {
    if (classFileManagerType.isPresent) {
      classFileManagerType.get match {
        case _: DeleteImmediatelyManagerType => new DeleteClassFileManager
        case m: TransactionalManagerType =>
          transactional(m.backupDirectory, m.logger)
      }
    } else new DeleteClassFileManager
  }

  def getClassFileManager(options: IncOptions): XClassFileManager = {
    import sbt.internal.inc.JavaInterfaceUtil.{ EnrichOptional, EnrichOption }
    val internal = getDefaultClassFileManager(options.classfileManagerType)
    val external = Option(options.externalHooks())
      .flatMap(ext => ext.getExternalClassFileManager.toOption)
    xsbti.compile.WrappedClassFileManager.of(internal, external.toOptional)
  }

  private final class DeleteClassFileManager extends XClassFileManager {

    override def delete(classes: Array[File]): Unit = {
      val (jared, regular) = splitToClassesAndJars(classes)
      // regular
      IO.deleteFilesEmptyDirs(regular)
      // jared
      groupByJars(jared).foreach {
        case (jar, classes) =>
          STJUtil.removeFromJar(jar, classes)
      }
    }

    override def generated(classes: Array[File]): Unit = ()
    override def complete(success: Boolean): Unit = ()
  }

  /**
   * Constructs a minimal [[ClassFileManager]] implementation that immediately deletes
   * class files when they are requested. This is the default implementation of the class
   * file manager by the Scala incremental compiler if no class file manager is specified.
   */
  def deleteImmediately: XClassFileManager = new DeleteClassFileManager

  /**
   * Constructs a transactional [[ClassFileManager]] implementation that restores class
   * files to the way they were before compilation if there is an error. Otherwise, it
   * keeps the successfully generated class files from the new compilation.
   *
   * This is the default class file manager used by sbt, and makes sense in a lot of scenarios.
   */
  def transactional(tempDir0: File, logger: sbt.util.Logger): XClassFileManager =
    new TransactionalClassFileManager(tempDir0, logger)

  private final class TransactionalClassFileManager(tempDir0: File, logger: sbt.util.Logger)
      extends XClassFileManager {
    val tempDir = tempDir0.getCanonicalFile
    IO.delete(tempDir)
    IO.createDirectory(tempDir)
    logger.debug(s"Created transactional ClassFileManager with tempDir = $tempDir")

    private[this] val generatedClasses = new mutable.HashSet[File]
    private[this] val movedClasses = new mutable.HashMap[File, File]
    private[this] val movedJaredClasses = new mutable.HashMap[STJUtil.JaredClass, URI]
    private[this] val realToTmpJars = new mutable.HashMap[URI, URI]

    private def showFiles(files: Iterable[File]): String = {
      if (files.isEmpty) {
        "[]"
      } else {
        files.map(f => s"\t$f").mkString("\n")
      }
    }

    override def delete(classes: Array[File]): Unit = {
      show(s"About to delete class files:\n${showFiles(classes)}")
      val (jared, regular) = splitToClassesAndJars(classes)

      // old logic for regular class files
      locally {
        val toBeBackedUp =
          regular.filter(c => c.exists && !movedClasses.contains(c) && !generatedClasses(c))
        show(s"We backup class files:\n${showFiles(toBeBackedUp)}")
        for (c <- toBeBackedUp) {
          movedClasses.put(c, move(c))
        }
        IO.deleteFilesEmptyDirs(regular)
      }

      // logic for jared classes
      locally {
        val toBeBackedUp = jared.filter { jc =>
          STJUtil.existsInJar(jc) && !movedJaredClasses.contains(jc) && !generatedClasses.contains(
            new File(jc))
        }
        show(s"We backup jared class files:\n${showFiles(toBeBackedUp.map(new File(_)))}")
        groupByJars(toBeBackedUp).foreach {
          case (jar, classes) =>
            // backup
            def newTmpJar =
              STJUtil.fileToJarUri(new File(tempDir, UUID.randomUUID.toString + ".jar"))
            val targetJar = realToTmpJars.getOrElse(jar, newTmpJar)
            for (c <- classes) {
              movedJaredClasses.put(STJUtil.fromJarUriAndRelClass(jar, c), targetJar)
              STJUtil.withZipFs(jar) { srcFs =>
                STJUtil.withZipFs(targetJar, create = true) { dstFs =>
                  val src = srcFs.getPath(c)
                  val dst = dstFs.getPath(c)
                  if (!Files.isDirectory(src)) {
                    Files.createDirectories(dst.getParent)
                  }
                  Files.copy(src, dst, StandardCopyOption.COPY_ATTRIBUTES)
                }
              }
            }
            // remove
            show(s"Removing ${classes.toList.mkString("\n")} from $jar")
            STJUtil.removeFromJar(jar, classes)
        }
      }
    }

    override def generated(classes: Array[File]): Unit = {
      show(s"Registering generated classes:\n${showFiles(classes)}")
      generatedClasses ++= classes
      ()
    }

    override def complete(success: Boolean): Unit = {
      if (!success) {
        show("Rolling back changes to class files.")
        show(s"Removing generated classes:\n${showFiles(generatedClasses)}")
        locally {
          val (jared, regular) = splitToClassesAndJars(generatedClasses)
          // regular
          IO.deleteFilesEmptyDirs(regular)
          // jared
          groupByJars(jared).foreach {
            case (jar, classes) => STJUtil.removeFromJar(jar, classes)
          }
        }

        // regular
        show(s"Restoring class files: \n${showFiles(movedClasses.keys)}")
        for ((orig, tmp) <- movedClasses) IO.move(tmp, orig)

        // jared
        val toMove = movedJaredClasses.toSeq.map {
          case (jaredClass, tmpJar) =>
            val srcJar = STJUtil.jaredClassToJarFile(jaredClass)
            (srcJar, STJUtil.jarUriToFile(tmpJar))
        }.distinct
        show(
          s"Restoring jared class files: \n${showFiles(movedJaredClasses.keys.map(new File(_)))}")
        toMove.foreach {
          case (srcJar, tmpJar) =>
            val tmpSrc = tmpJar.toPath.resolveSibling("~~tmp~cfm~merge~~.jar").toFile
            Files.copy(srcJar.toPath, tmpSrc.toPath, StandardCopyOption.REPLACE_EXISTING)
            STJUtil.mergeJars(into = tmpSrc, from = tmpJar)
            Files.move(tmpSrc.toPath, srcJar.toPath, StandardCopyOption.REPLACE_EXISTING)
        }
      }
      logger.debug(s"Removing the temporary directory used for backing up class files: $tempDir")
      IO.delete(tempDir)
    }

    def move(c: File): File = {
      val target = File.createTempFile("sbt", ".class", tempDir)
      IO.move(c, target)
      target
    }

    private def show(a: String): Unit = {
      println("~~[CFM] " + a)
    }
  }

  private def groupByJars(
      jared: Iterable[STJUtil.JaredClass]): Map[URI, Iterable[STJUtil.RelClass]] = {
    jared
      .map(jc => STJUtil.toJarUriAndRelClass(jc))
      .groupBy(_._1)
      .mapValues(_.map(_._2))
  }

  private def splitToClassesAndJars(
      classes: Iterable[File]): (Iterable[STJUtil.JaredClass], Iterable[File]) = {
    val (jars, classFiles) = classes.partition(STJUtil.isJar)
    (jars.map(_.toString), classFiles)
  }

}
