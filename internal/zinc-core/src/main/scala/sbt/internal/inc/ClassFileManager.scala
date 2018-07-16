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
      IO.deleteFilesEmptyDirs(regular)
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
    private[this] val movedJaredClasses = new mutable.HashMap[File, URI]
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
        val toBeBackedUp =
          jared.filter(
            c =>
              STJUtil.existsInJar(c.toString) && !movedJaredClasses.contains(c) && !generatedClasses
                .contains(c))
        show(s"We backup jared class files:\n${showFiles(toBeBackedUp)}")

        groupByJars(toBeBackedUp).foreach {
          case (jar, classes) =>
            val targetJar = realToTmpJars.getOrElse(
              jar,
              STJUtil.fileToJarUri(new File(tempDir, UUID.randomUUID.toString + ".jar")))
            // copy to target jar all classes
            for (c <- classes) {
              movedJaredClasses.put(new File(STJUtil.fromJarUriAndFile(jar, c)), targetJar)

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
            // maybe "move" should be handled as I am copying but probably handled by next line
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

    private def show(a: String): Unit = {
      println("~~[CFM] " + a)
    }

    override def complete(success: Boolean): Unit = {
      if (!success) {
        show("Rolling back changes to class files.")
        show(s"Removing generated classes:\n${showFiles(generatedClasses)}")
        locally {
          val (jared, regular) = splitToClassesAndJars(generatedClasses)
          IO.deleteFilesEmptyDirs(regular)
          groupByJars(jared).foreach {
            case (jar, classes) => STJUtil.removeFromJar(jar, classes)
          }
        }

        show(s"Restoring class files: \n${showFiles(movedClasses.keys)}")
        for ((orig, tmp) <- movedClasses) IO.move(tmp, orig)

        ///
        val toMove = movedJaredClasses.toSeq.map {
          case (srcFile, tmpJar) =>
            // jar# to file
            val srcJar = STJUtil.rawIdToJarFile(srcFile.toString)
            (srcJar, STJUtil.jarUriToFile(tmpJar))
        }.distinct
        show(s"Restoring jared class files: \n${showFiles(movedJaredClasses.keys)}")
        toMove.foreach {
          case (srcJar, tmpJar) =>
            val tmpSrc = tmpJar.toPath.resolveSibling("~~tmp~cfm~merge~~.jar").toFile
            Files.copy(srcJar.toPath, tmpSrc.toPath)
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
  }

  private def groupByJars(jared: Iterable[File]): Map[URI, Iterable[String]] = {
    jared
      .map(f => STJUtil.toJarUriAndFile(f.toString))
      .groupBy(_._1)
      .mapValues(_.map(_._2))
  }

  private def splitToClassesAndJars(classes: Iterable[File]) = {
    classes.partition(STJUtil.isJar)
  }

}
