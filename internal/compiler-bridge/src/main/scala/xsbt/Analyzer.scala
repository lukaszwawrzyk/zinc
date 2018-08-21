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

    private lazy val existingClassFiles: Set[STJ.JaredClass] = {
      STJ.outputJar
        .map { jar =>
          val classes = STJ.listFiles(jar)
          classes.map(STJ.init(jar, _))
        }
        .getOrElse(Set.empty)
    }

    def apply(unit: CompilationUnit): Unit = {
      if (!unit.isJava) {
        val sourceFile = unit.source.file.file
        // build list of generated classes
        for (iclass <- unit.icode) {
          val sym = iclass.symbol
          def addGenerated(separatorRequired: Boolean): Unit = {
            val locatedClass = if (STJ.enabled) {
              locateClassInJar(sym, separatorRequired)
            } else {
              locatePlainClassFile(sym, separatorRequired)
            }

            locatedClass
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

    private def locatePlainClassFile(sym: Symbol, separatorRequired: Boolean): Option[File] = {
      outputDirs
        .map(fileForClass(_, sym, separatorRequired))
        .find(_.exists())
    }

    private def locateClassInJar(sym: Symbol, separatorRequired: Boolean): Option[File] = {
      val classFile =
        fileForClass(new java.io.File("."), sym, separatorRequired).toString
          .drop(2) // stripPrefix ./ or .\
      val jaredClass = STJ.init(classFile)
      if (existingClassFiles.contains(jaredClass)) {
        Some(new File(jaredClass))
      } else {
        None
      }
    }
  }

  private object STJ {
    type JaredClass = String
    type RelClass = String

    def init(jar: File, cls: RelClass): JaredClass = {
      // This identifier will be stored as a java.io.File. Its constructor will normalize slashes
      // which means that the identifier to be consistent should at all points have consistent
      // slashes for safe comparisons, especially in sets or maps.
      val relClass = if (File.separatorChar == '/') cls else cls.replace(File.separatorChar, '/')
      s"$jar!$relClass"
    }

    def init(cls: RelClass): JaredClass = {
      init(STJ.outputJar.get, cls)
    }

    def listFiles(jar: File): Set[RelClass] = {
      import scala.collection.JavaConverters._
      // ZipFile is slightly slower than IndexBasedZipFsOps but it is quite difficult to use reuse
      // IndexBasedZipFsOps in compiler bridge.
      val zip = new ZipFile(jar)
      try {
        zip.entries().asScala.filterNot(_.isDirectory).map(_.getName).toSet
      } finally {
        zip.close()
      }
    }

    val outputJar: Option[File] = {
      outputDirs match {
        case Seq(file) if file.getName.endsWith("jar") => Some(file)
        case _                                         => None
      }
    }

    val enabled: Boolean = outputJar.isDefined
  }

}
