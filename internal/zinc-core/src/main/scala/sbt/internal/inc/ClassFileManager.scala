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
import java.nio.file.{ Files, StandardCopyOption, Path }
import java.util.{ UUID, Optional }

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
    override def delete(classes: Array[File]): Unit =
      IO.deleteFilesEmptyDirs(classes)
    override def generated(classes: Array[File]): Unit = ()
    override def complete(success: Boolean): Unit = ()
  }

  // todo merge with above class
  private final class JaredDeleteClassFileManager extends XClassFileManager {
    override def delete(classes: Array[File]): Unit = {
      groupByJars(classes.map(_.toString)).foreach {
        case (jar, classes) =>
          STJ.removeFromJar(jar, classes)
      }
    }
    override def generated(classes: Array[File]): Unit = ()
    override def complete(success: Boolean): Unit = ()

    private def groupByJars(jared: Iterable[STJ.JaredClass]): Map[File, Iterable[STJ.RelClass]] = {
      jared
        .map(jc => STJ.toJarAndRelClass(jc))
        .groupBy(_._1)
        .mapValues(_.map(_._2))
    }

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

  def jarBasedTransactional(outputJar: File): XClassFileManager = {
    new JarBasedTransactionalClassFileManager(outputJar.toPath)
  }

  private final class TransactionalClassFileManager(tempDir0: File, logger: sbt.util.Logger)
      extends XClassFileManager {
    val tempDir = tempDir0.getCanonicalFile
    IO.delete(tempDir)
    IO.createDirectory(tempDir)
    logger.debug(s"Created transactional ClassFileManager with tempDir = $tempDir")

    private[this] val generatedClasses = new mutable.HashSet[File]
    private[this] val movedClasses = new mutable.HashMap[File, File]

    private def showFiles(files: Iterable[File]): String =
      files.map(f => s"\t$f").mkString("\n")

    override def delete(classes: Array[File]): Unit = {
      logger.debug(s"About to delete class files:\n${showFiles(classes)}")
      val toBeBackedUp =
        classes.filter(c => c.exists && !movedClasses.contains(c) && !generatedClasses(c))
      logger.debug(s"We backup class files:\n${showFiles(toBeBackedUp)}")
      for (c <- toBeBackedUp) {
        movedClasses.put(c, move(c))
      }
      IO.deleteFilesEmptyDirs(classes)
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
        IO.deleteFilesEmptyDirs(generatedClasses)
        logger.debug(s"Restoring class files: \n${showFiles(movedClasses.keys)}")
        for ((orig, tmp) <- movedClasses) IO.move(tmp, orig)
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

  private final class JarBasedTransactionalClassFileManager(outputJar: Path)
      extends XClassFileManager {

    val backedUpJar: Option[Path] = {
      if (Files.exists(outputJar)) {
        val backupPath = outputJar.resolveSibling(s"${outputJar.getFileName}.bak")
        Files.copy(outputJar, backupPath, StandardCopyOption.REPLACE_EXISTING)
        Some(backupPath)
      } else {
        None
      }
    }

    override def delete(jaredClasses: Array[File]): Unit = {
      val classes = jaredClasses.map(s => STJ.toRelClass(s.toString))
      STJ.removeFromJar(outputJar.toFile, classes)
    }

    override def generated(classes: Array[File]): Unit = ()

    override def complete(success: Boolean): Unit = {
      if (!success) {
        backedUpJar.foreach(backup =>
          Files.move(backup, outputJar, StandardCopyOption.REPLACE_EXISTING))
      }
    }
  }
}
