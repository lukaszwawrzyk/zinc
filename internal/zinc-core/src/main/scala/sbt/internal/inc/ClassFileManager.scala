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
  IncOptions,
  DeleteImmediatelyManagerType,
  TransactionalManagerType,
  ClassFileManagerType,
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
      IO.deleteFilesEmptyDirs(regular)
      groupByJars(jared).foreach {
        case (jar, classes) =>
          removeFromZip(jar, classes)
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
    private[this] val movedJaredClasses = new mutable.HashMap[File, File]
    private[this] val realToTmpJars = new mutable.HashMap[URI, URI]

    private def showFiles(files: Iterable[File]): String =
      files.map(f => s"\t$f").mkString("\n")

    override def delete(classes: Array[File]): Unit = {
      println(s"About to delete class files:\n${showFiles(classes)}")
      val (jared, regular) = splitToClassesAndJars(classes)

      locally {
        val toBeBackedUp =
          regular.filter(c => c.exists && !movedClasses.contains(c) && !generatedClasses(c))
        println(s"We backup class files:\n${showFiles(toBeBackedUp)}")
        for (c <- toBeBackedUp) {
          movedClasses.put(c, move(c))
        }
        IO.deleteFilesEmptyDirs(regular)
      }

      locally {
        val toBeBackedUp =
          jared.filter(c => !movedJaredClasses.contains(c) && !generatedClasses.contains(c))
        println(s"We backup jared class files:\n${showFiles(toBeBackedUp)}")

        groupByJars(toBeBackedUp).foreach {
          case (jar, classes) =>
            val targetJar = realToTmpJars.getOrElse(
              jar,
              URI.create(
                "jar:file:" + new File(tempDir, UUID.randomUUID.toString + ".jar").toString))
            // copy to target jar all classes
            for (c <- classes) {
              movedJaredClasses.put(new File(jar.toString + "!" + c), new File(targetJar.toString))

              STJUtil.withZipFs(jar) { srcFs =>
                STJUtil.withZipFs(targetJar, create = true) { destFs =>
                  Files.copy(srcFs.getPath(c), destFs.getPath(c))
                }
              }
            }
            // maybe "move" should be handled as I am copying but probably handled by next line
            println(s"Removing ${classes.toList.mkString("\n")} from $jar")
            removeFromZip(jar, classes)
        }
      }
    }

    override def generated(classes: Array[File]): Unit = {
      logger.debug(s"Registering generated classes:\n${showFiles(classes)}")
      generatedClasses ++= classes
      ()
    }

    override def complete(success: Boolean): Unit = {
      if (!success) {
        logger.debug("Rolling back changes to class files.")
        logger.debug(s"Removing generated classes:\n${showFiles(generatedClasses)}")
        locally {
          val (jared, regular) = splitToClassesAndJars(generatedClasses)
          IO.deleteFilesEmptyDirs(regular)
          groupByJars(jared).foreach {
            case (jar, classes) => removeFromZip(jar, classes)
          }
        }

        logger.debug(s"Restoring class files: \n${showFiles(movedClasses.keys)}")
        for ((orig, tmp) <- movedClasses) IO.move(tmp, orig)

        val toMove = movedJaredClasses.toSeq.map {
          case (srcFile, tmpJar) =>
            val srcJar = new File(srcFile.toString.split("!")(0).stripPrefix("jar:file:"))
            (srcJar, new File(tmpJar.toString.stripPrefix("jar:file:")))
        }.distinct
        toMove.foreach {
          case (srcJar, tmpJar) =>
            STJUtil.mergeJars(into = srcJar, from = tmpJar)
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
  }

  private def removeFromZip(zip: URI, classes: Iterable[String]): Unit = {
    try {
      val env = new java.util.HashMap[String, String]
      val fs = FileSystems.newFileSystem(zip, env)
      classes.foreach { cls =>
        Files.delete(fs.getPath(cls))
      }
      fs.close()
    } catch {
      case _: FileSystemNotFoundException =>
      // file does not exist due to compilation error
      // if it would be created we would need to remove all entries anyway.
    }
  }

  private def groupByJars(jared: Iterable[File]): Map[URI, Iterable[String]] = {
    jared
      .map(_.toString.split("!"))
      .collect { case Array(jar, classFile) => (jar, classFile) }
      .groupBy(_._1)
      .mapValues(_.map(_._2))
      .map { case (k, v) => URI.create(k) -> v }
  }

  private def splitToClassesAndJars(classes: Iterable[File]) = {
    classes.partition(_.toString.startsWith("jar:file:"))
  }

}
