/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package xsbt

import java.io.File
import java.util.zip.ZipFile

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
      val classFile =
        fileForClass(new java.io.File("."), sym, separatorRequired).toString
          .drop(2) // stripPrefix ./ or .\
      val jaredClass = STJUtil.init(jarFile, classFile)
      if (STJUtil.existsInJar(jaredClass)) {
        // scalac is compiling to a temporary jar to avoid locking problems
        // but in analysis output we want the actual, final jar. Its name
        // is encoded in temp jar name after the separator
        val finalJarName = jarFile.getName.split("_tmpjarsep_")(1)
        val finalJarPath = jarFile.toPath.resolveSibling(finalJarName).toFile
        val finalUri = STJUtil.init(finalJarPath, classFile)
        Some(new File(finalUri))
      } else {
        None
      }
    }.headOption
  }

  private object STJUtil {
    type JaredClass = String

    def isWindows: Boolean = System.getProperty("os.name").toLowerCase.contains("win")

    def init(jar: File, cls: String): String = {
      // to ensure consistent 'slashing' as those identifies are used e.g. in maps
      val classPath = if (isWindows) cls.replace("/", "\\") else cls
      s"$jar!$classPath"
    }

    def toJarAndRelClass(c: JaredClass): (File, String) = {
      val Array(jar, relClass) = c.split("!")
      // paths within jars always have forward slashes but JaredClass has system defined slashes
      // because it is often stored in File that controls the slashes
      val fixedRelClass = relClass.replace("\\", "/")
      (new File(jar), fixedRelClass)
    }

    def existsInJar(s: String): Boolean = {
      val (jar, cls) = toJarAndRelClass(s)
      if (jar.exists()) {
        val file = new ZipFile(jar, ZipFile.OPEN_READ)
        val exists = file.getEntry(cls) != null
        file.close()
        exists
      } else {
        false
      }
    }
  }

}
