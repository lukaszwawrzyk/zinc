/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package xsbt

import java.io.File
import java.net.URI
import java.nio.file.{ FileSystems, FileSystem, Files }

import scala.tools.nsc.Phase

object Analyzer {
  def name = "xsbt-analyzer"
}
final class Analyzer(val global: CallbackGlobal) extends LocateClassFile {
  import global._

  def newPhase(prev: Phase): Phase = new AnalyzerPhase(prev)
  private class AnalyzerPhase(prev: Phase) extends GlobalPhase(prev) {
    override def description =
      "Finds concrete instances of provided superclasses, and application entry points."
    def name = Analyzer.name
    def apply(unit: CompilationUnit): Unit = {
      if (!unit.isJava) {
        val sourceFile = unit.source.file.file
        // build list of generated classes
        for (iclass <- unit.icode) {
          val sym = iclass.symbol
          def addGenerated(separatorRequired: Boolean): Unit = {
            locatePlainClassFile(sym, separatorRequired)
              .orElse(locateClassInJar(sym, separatorRequired))
              .foreach { classFile =>
                assert(sym.isClass, s"${sym.fullName} is not a class")
                // we would like to use Symbol.isLocalClass but that relies on Symbol.owner which
                // is lost at this point due to lambdalift
                // the LocalNonLocalClass.isLocal can return None, which means, we're asking about
                // the class it has not seen before. How's that possible given we're performing a lookup
                // for every declared class in Dependency phase? We can have new classes introduced after
                // Dependency phase has ran. For example, the implementation classes for traits.
                val isLocalClass = localToNonLocalClass.isLocal(sym).getOrElse(true)
                if (!isLocalClass) {
                  val srcClassName = classNameAsString(sym)
                  val binaryClassName = flatclassName(sym, '.', separatorRequired)
                  callback.generatedNonLocalClass(sourceFile,
                                                  classFile,
                                                  binaryClassName,
                                                  srcClassName)
                } else {
                  callback.generatedLocalClass(sourceFile, classFile)
                }
              }
          }
          if (sym.isModuleClass && !sym.isImplClass) {
            if (isTopLevelModule(sym) && sym.companionClass == NoSymbol)
              addGenerated(false)
            addGenerated(true)
          } else
            addGenerated(false)
        }
      }
    }
  }

  private def locatePlainClassFile(sym: Symbol, separatorRequired: Boolean): Option[File] = {
    outputDirs
      .map(fileForClass(_, sym, separatorRequired))
      .find(_.exists)
  }

  private def locateClassInJar(sym: Symbol, separatorRequired: Boolean): Option[File] = {
    outputDirs.flatMap { jarFile =>
      val relativeFile =
        fileForClass(new java.io.File("."), sym, separatorRequired).toString.drop(2).replace("\\", "/")
      val uri = "jar:" + jarFile.toPath.toUri.toString + "!/" + relativeFile
      val file = new File(uri)
      if (existsInJar(uri)) {
        Some(file)
      } else {
        None
      }
    }.headOption
  }

  private def existsInJar(uri: String): Boolean = {
    val Array(jar, cls) = uri.split("!")
    STJUtil.withZipFs(URI.create(jar)) { fs: FileSystem =>
      Files.exists(fs.getPath(cls))
    }
  }

  private object STJUtil {
    def withZipFs[A](uri: URI)(action: FileSystem => A): A = {
      val env = new java.util.HashMap[String, String]
      synchronized {
        val fs = FileSystems.newFileSystem(uri, env)
        try action(fs)
        finally fs.close()
      }
    }

  }

}
