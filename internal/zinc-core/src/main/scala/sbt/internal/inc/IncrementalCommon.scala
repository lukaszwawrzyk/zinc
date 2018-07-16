/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package sbt
package internal
package inc

import java.io.File
import java.nio.file.{ Files, StandardCopyOption, StandardOpenOption }
import java.util.UUID

import sbt.io.IO
import xsbti.api.AnalyzedClass
import xsbti.compile.{
  Changes,
  CompileAnalysis,
  DependencyChanges,
  IncOptions,
  Output,
  ClassFileManager => XClassFileManager
}
import xsbti.compile.analysis.{ ReadStamps, Stamp => XStamp }

import scala.annotation.tailrec
import scala.util.{ Random, Try }

private[inc] abstract class IncrementalCommon(val log: sbt.util.Logger, options: IncOptions) {

  import STJUtil.pause

  // setting the related system property to true will skip checking that the class name
  // still comes from the same classpath entry.  This can workaround bugs in classpath construction,
  // such as the currently problematic -javabootclasspath.  This is subject to removal at any time.
  private[this] def skipClasspathLookup = java.lang.Boolean.getBoolean("xsbt.skip.cp.lookup")

  val wrappedLog = new Incremental.PrefixingLogger("[inv] ")(log)
  def debug(s: => String) = if (options.relationsDebug) wrappedLog.debug(s) else ()

  // TODO: the Analysis for the last successful compilation should get returned + Boolean indicating success
  // TODO: full external name changes, scopeInvalidations
  @tailrec final def cycle(invalidatedRaw: Set[String],
                           modifiedSrcs: Set[File],
                           allSources: Set[File],
                           binaryChanges: DependencyChanges,
                           lookup: ExternalLookup,
                           previous: Analysis,
                           doCompile: (Set[File], DependencyChanges, Seq[File], File) => Analysis,
                           output: Output,
                           classfileManager: XClassFileManager,
                           cycleNum: Int): Analysis =
    if (invalidatedRaw.isEmpty && modifiedSrcs.isEmpty)
      previous
    else {
      debugOuterSection(s"Recompilation cycle #$cycleNum")
      val invalidatedPackageObjects =
        this.invalidatedPackageObjects(invalidatedRaw, previous.relations, previous.apis)
      if (invalidatedPackageObjects.nonEmpty)
        log.debug(s"Invalidated package objects: $invalidatedPackageObjects")
      val withPackageObjects = invalidatedRaw ++ invalidatedPackageObjects
      val invalidatedClasses = withPackageObjects

      val (current, recompiledRecently) = recompileClasses(invalidatedClasses,
                                                           modifiedSrcs,
                                                           allSources,
                                                           binaryChanges,
                                                           previous,
                                                           doCompile,
                                                           output,
                                                           classfileManager)

      // If we recompiled all sources no need to check what is changed since there is nothing more to recompile
      if (recompiledRecently == allSources) current
      else {
        // modifiedSrc have to be mapped to class names both of previous and current analysis because classes might be
        // removed (it's handled by `previous`) or added (it's handled by `current`) or renamed (it's handled by both)
        val recompiledClasses = invalidatedClasses ++
          modifiedSrcs.flatMap(previous.relations.classNames) ++ modifiedSrcs.flatMap(
          current.relations.classNames)

        val incChanges =
          changedIncremental(recompiledClasses, previous.apis.internalAPI, current.apis.internalAPI)

        debug("\nChanges:\n" + incChanges)
        val transitiveStep = options.transitiveStep
        val classToSourceMapper = new ClassToSourceMapper(previous.relations, current.relations)
        val incrementallyInvalidated = invalidateIncremental(
          current.relations,
          current.apis,
          incChanges,
          recompiledClasses,
          cycleNum >= transitiveStep,
          classToSourceMapper.isDefinedInScalaSrc)
        val allInvalidated =
          if (lookup.shouldDoIncrementalCompilation(incrementallyInvalidated, current))
            incrementallyInvalidated
          else Set.empty[String]

        cycle(allInvalidated,
              Set.empty,
              allSources,
              emptyChanges,
              lookup,
              current,
              doCompile,
              output,
              classfileManager,
              cycleNum + 1)
      }
    }

  private[this] def recompileClasses(
      classes: Set[String],
      modifiedSrcs: Set[File],
      allSources: Set[File],
      binaryChanges: DependencyChanges,
      previous: Analysis,
      doCompile: (Set[File], DependencyChanges, Seq[File], File) => Analysis,
      output: Output,
      classfileManager: XClassFileManager): (Analysis, Set[File]) = {
    val invalidatedSources = classes.flatMap(previous.relations.definesClass) ++ modifiedSrcs
    val invalidatedSourcesForCompilation = expand(invalidatedSources, allSources)
    pause("Before prune")
    val pruned = Incremental.prune(invalidatedSourcesForCompilation, previous, classfileManager)
    pause("After prune")
    debugInnerSection("Pruned")(pruned.relations)

    val fresh = withPreviousJar(output) {
      doCompile(invalidatedSourcesForCompilation, binaryChanges, _, _)
    }

    // For javac as class files are added to classfileManager as they are generated, so
    // this step is redundant. For scalac this is still necessary. TODO: do the same for scalac.
    classfileManager.generated(fresh.relations.allProducts.toArray)
    val merged = pruned ++ fresh //.copy(relations = pruned.relations ++ fresh.relations, apis = pruned.apis ++ fresh.apis)

    if (fresh.relations == merged.relations) {
      debugInnerSection("Fresh [== Merged]")(fresh.relations)
    } else {
      debugInnerSection("Fresh")(fresh.relations)
      debugInnerSection("Merged")(merged.relations)
    }
    (merged, invalidatedSourcesForCompilation)
  }

  private def withPreviousJar[A](output: Output)(action: (Seq[File], File) => A): A = {
    val outputJar = output.getSingleOutput.get

    // cleanup stuff from other compilations
    pause("Trying to get rid of tmpjars")
    Option(outputJar.toPath.getParent.toFile.listFiles()).foreach { files =>
      files
        .filter(f => f.getName.endsWith(".jar") && f.getName.startsWith("tmpjar"))
        .foreach(f => Try(f.delete()))
    }

    val prevJar = outputJar.toPath.resolveSibling("tmpjarprev" + UUID.randomUUID() + ".jar").toFile
    if (outputJar.exists()) {
      pause(s"Prev jar set as $prevJar output jar ($outputJar) exists so moving it ")
      IO.move(outputJar, prevJar)
      pause(s"Moved")
    }
    val jarForStupidScalac =
      outputJar.toPath
        .resolveSibling("tmpjarout" + UUID.randomUUID() + "_tmpjarsep_" + outputJar.getName)
        .toFile
    pause(s"About to run compilation, will enforce $jarForStupidScalac as output")
    val res = try action(Seq(prevJar), jarForStupidScalac)
    catch {
      case e: Exception =>
        pause("Compilation failed")
        if (prevJar.exists()) {
          pause(s"Reverting prev jar $prevJar onto $outputJar")
          IO.move(prevJar, outputJar)
          pause("Reverted prev jar")
        }
        throw e
    }

    if (prevJar.exists()) {
      if (jarForStupidScalac.exists()) {
        // most complex case: scala compilation completed to a temp jar and prev jars exists, they need to be merged
        val tmpTargetJar = prevJar.toPath.resolveSibling("~~merge~target~~.jar")
        val tmpSrcJar = jarForStupidScalac.toPath.resolveSibling("~~merge~source~~.jar")
        pause(
          s"Prev jar and temp out jar exist so merging those, will copy $prevJar on $tmpTargetJar")
        Files.copy(prevJar.toPath, tmpTargetJar)
        pause(s"Will copy $jarForStupidScalac on $tmpSrcJar")
        Files.copy(jarForStupidScalac.toPath, tmpSrcJar)
        pause(s"Prev jar and out jar exist so merging those $tmpTargetJar and $tmpSrcJar")
        STJUtil.mergeJars(into = tmpTargetJar.toFile, from = tmpSrcJar.toFile)
        pause(s"merged, moving prevJar on outJar $tmpTargetJar to $outputJar")
        IO.move(tmpTargetJar.toFile, outputJar)
        pause(s"moved, trying to remove $jarForStupidScalac")
        Try(Files.delete(jarForStupidScalac.toPath)) // probably will fail anyway
        pause("Finally done")
      } else {
        // Java only compilation case - probably temporary as java should go to jar as well
        pause("java path")
        IO.move(prevJar, outputJar)
      }
    } else {
      if (jarForStupidScalac.exists()) {
        // prev jar does not exist so it is the first compilation for scalac, just rename temp jar to output
        pause(s"Copying $jarForStupidScalac to $outputJar")
        // copy is safer, will see
        Files.copy(jarForStupidScalac.toPath,
                   outputJar.toPath,
                   StandardCopyOption.COPY_ATTRIBUTES,
                   StandardCopyOption.REPLACE_EXISTING)
      } else {
        // there is no output jar, so it was a java compilation without prev jar, nothing to do
      }
    }

    res
  }

  private[this] def emptyChanges: DependencyChanges = new DependencyChanges {
    val modifiedBinaries = new Array[File](0)
    val modifiedClasses = new Array[String](0)
    def isEmpty = true
  }
  private[this] def expand(invalidated: Set[File], all: Set[File]): Set[File] = {
    val recompileAllFraction = options.recompileAllFraction
    if (invalidated.size > all.size * recompileAllFraction) {
      log.debug(
        "Recompiling all " + all.size + " sources: invalidated sources (" + invalidated.size + ") exceeded " + (recompileAllFraction * 100.0) + "% of all sources")
      all ++ invalidated // need the union because all doesn't contain removed sources
    } else invalidated
  }

  protected def invalidatedPackageObjects(invalidatedClasses: Set[String],
                                          relations: Relations,
                                          apis: APIs): Set[String]

  /**
   * Logs API changes using debug-level logging. The API are obtained using the APIDiff class.
   *
   * NOTE: This method creates a new APIDiff instance on every invocation.
   */
  private def logApiChanges(apiChanges: Iterable[APIChange],
                            oldAPIMapping: String => AnalyzedClass,
                            newAPIMapping: String => AnalyzedClass): Unit = {
    val contextSize = options.apiDiffContextSize
    try {
      val wrappedLog = new Incremental.PrefixingLogger("[diff] ")(log)
      val apiDiff = new APIDiff
      apiChanges foreach {
        case APIChangeDueToMacroDefinition(src) =>
          wrappedLog.debug(
            s"Public API is considered to be changed because $src contains a macro definition.")
        case apiChange: NamesChange =>
          val src = apiChange.modifiedClass
          val oldApi = oldAPIMapping(src)
          val newApi = newAPIMapping(src)
          val apiUnifiedPatch =
            apiDiff.generateApiDiff(src.toString, oldApi.api, newApi.api, contextSize)
          wrappedLog.debug(s"Detected a change in a public API ($src):\n$apiUnifiedPatch")
      }
    } catch {
      case e: Exception =>
        log.error("An exception has been thrown while trying to dump an api diff.")
        log.trace(e)
    }
  }

  /**
   * Accepts the classes that were recompiled during the last step and functions
   * providing the API before and after the last step.  The functions should return
   * an empty API if the class did not/does not exist.
   */
  def changedIncremental(
      lastClasses: collection.Set[String],
      oldAPI: String => AnalyzedClass,
      newAPI: String => AnalyzedClass
  ): APIChanges = {
    val apiChanges = lastClasses.flatMap { className =>
      sameClass(className, oldAPI(className), newAPI(className))
    }

    if (Incremental.apiDebug(options) && apiChanges.nonEmpty) {
      logApiChanges(apiChanges, oldAPI, newAPI)
    }

    new APIChanges(apiChanges)
  }

  def sameClass(className: String, a: AnalyzedClass, b: AnalyzedClass): Option[APIChange] = {
    // Clients of a modified class (ie, one that doesn't satisfy `shortcutSameClass`) containing macros must be recompiled.
    val hasMacro = a.hasMacro || b.hasMacro
    if (shortcutSameClass(a, b)) {
      None
    } else {
      if (hasMacro && IncOptions.getRecompileOnMacroDef(options)) {
        Some(APIChangeDueToMacroDefinition(className))
      } else sameAPI(className, a, b)
    }
  }

  protected def sameAPI(className: String, a: AnalyzedClass, b: AnalyzedClass): Option[APIChange]

  def shortcutSameClass(a: AnalyzedClass, b: AnalyzedClass): Boolean =
    a.compilationTimestamp() == b.compilationTimestamp() && (a.apiHash == b.apiHash)

  def changedInitial(sources: Set[File],
                     previousAnalysis0: CompileAnalysis,
                     current: ReadStamps,
                     lookup: Lookup)(implicit equivS: Equiv[XStamp]): InitialChanges = {
    val previousAnalysis = previousAnalysis0 match { case a: Analysis => a }
    val previous = previousAnalysis.stamps
    val previousRelations = previousAnalysis.relations
    val previousAPIs = previousAnalysis.apis

    val srcChanges = lookup.changedSources(previousAnalysis).getOrElse {
      def sourceModified(f: File): Boolean =
        !equivS.equiv(previous.source(f), current.source(f))
      changes(previous.allSources.toSet, sources, sourceModified _)
    }

    val removedProducts = lookup.removedProducts(previousAnalysis).getOrElse {
      previous.allProducts
        .filter(p => {
          val res = !equivS.equiv(previous.product(p), current.product(p))
          println(s"comparing for $p ${previous.product(p)} and ${current.product(p)} = ${!res}")
          res
        })
        .toSet
    }

    val binaryDepChanges = lookup.changedBinaries(previousAnalysis).getOrElse {
      previous.allBinaries
        .filter(externalBinaryModified(lookup, previous, current, previousRelations))
        .toSet
    }

    val incrementalExtApiChanges = changedIncremental(previousAPIs.allExternals,
                                                      previousAPIs.externalAPI,
                                                      currentExternalAPI(lookup))
    val extApiChanges =
      if (lookup.shouldDoIncrementalCompilation(incrementalExtApiChanges.allModified.toSet,
                                                previousAnalysis)) incrementalExtApiChanges
      else new APIChanges(Nil)

    InitialChanges(srcChanges, removedProducts, binaryDepChanges, extApiChanges)
  }

  def changes(previous: Set[File],
              current: Set[File],
              existingModified: File => Boolean): Changes[File] = {
    new UnderlyingChanges[File] {
      private val inBoth = previous & current
      val removed = previous -- inBoth
      val added = current -- inBoth
      val (changed, unmodified) = inBoth.partition(existingModified)
    }
  }

  def invalidateIncremental(previous: Relations,
                            apis: APIs,
                            changes: APIChanges,
                            recompiledClasses: Set[String],
                            transitive: Boolean,
                            isScalaClass: String => Boolean): Set[String] = {
    val dependsOnClass = previous.memberRef.internal.reverse _
    val propagated: Set[String] =
      if (transitive)
        transitiveDependencies(dependsOnClass, changes.allModified.toSet)
      else
        invalidateIntermediate(previous, changes, isScalaClass)

    val dups = invalidateDuplicates(previous)
    if (dups.nonEmpty)
      log.debug("Invalidated due to generated class file collision: " + dups)

    val inv: Set[String] = propagated ++ dups
    val newlyInvalidated = (inv -- recompiledClasses) ++ dups
    log.debug(
      "All newly invalidated classes after taking into account (previously) recompiled classes:" + newlyInvalidated)
    if (newlyInvalidated.isEmpty) Set.empty else inv
  }

  /** Invalidate all classes that claim to produce the same class file as another class. */
  def invalidateDuplicates(merged: Relations): Set[String] =
    merged.srcProd.reverseMap.flatMap {
      case (_, sources) =>
        if (sources.size > 1) sources.flatMap(merged.classNames) else Nil
    }.toSet

  /**
   * Returns the transitive class dependencies of `initial`.
   * Because the intermediate steps do not pull in cycles, this result includes the initial classes
   * if they are part of a cycle containing newly invalidated classes.
   */
  def transitiveDependencies(dependsOnClass: String => Set[String],
                             initial: Set[String]): Set[String] = {
    val transitiveWithInitial = transitiveDeps(initial)(dependsOnClass)
    val transitivePartial = includeInitialCond(initial, transitiveWithInitial, dependsOnClass)
    log.debug("Final step, transitive dependencies:\n\t" + transitivePartial)
    transitivePartial
  }

  /** Invalidates classes and sources based on initially detected 'changes' to the sources, products, and dependencies.*/
  def invalidateInitial(previous: Relations, changes: InitialChanges): (Set[String], Set[File]) = {
    def classNames(srcs: Set[File]): Set[String] =
      srcs.flatMap(previous.classNames)
    def toImmutableSet(srcs: java.util.Set[File]): Set[File] = {
      import scala.collection.JavaConverters.asScalaIteratorConverter
      srcs.iterator().asScala.toSet
    }

    val srcChanges = changes.internalSrc
    val modifiedSrcs = toImmutableSet(srcChanges.getChanged)
    val addedSrcs = toImmutableSet(srcChanges.getAdded)
    val removedSrcs = toImmutableSet(srcChanges.getRemoved)
    val removedClasses = classNames(removedSrcs)
    val dependentOnRemovedClasses = removedClasses.flatMap(previous.memberRef.internal.reverse)
    val modifiedClasses = classNames(modifiedSrcs)
    val invalidatedClasses = removedClasses ++ dependentOnRemovedClasses ++ modifiedClasses
    val byProduct = changes.removedProducts.flatMap(previous.produced)
    val byBinaryDep = changes.binaryDeps.flatMap(previous.usesLibrary)
    val classToSrc = new ClassToSourceMapper(previous, previous)
    val byExtSrcDep = {
      //changes.external.modified.flatMap(previous.usesExternal) // ++ scopeInvalidations
      invalidateByAllExternal(previous, changes.external, classToSrc.isDefinedInScalaSrc)
    }

    checkAbsolute(addedSrcs.toList)

    val allInvalidatedClasses = invalidatedClasses ++ byExtSrcDep
    val allInvalidatedSourcefiles = addedSrcs ++ modifiedSrcs ++ byProduct ++ byBinaryDep

    debugOuterSection(s"Initial invalidation")
    if (previous.allSources.isEmpty)
      log.debug("Full compilation, no sources in previous analysis.")
    else if (allInvalidatedClasses.isEmpty && allInvalidatedSourcefiles.isEmpty)
      log.debug("No changes")
    else {
      def color(s: String) = Console.YELLOW + s + Console.RESET
      log.debug(
        s"""
           |${color("Initial source changes")}:
           |  ${color("removed")}: ${showSet(removedSrcs, baseIndent = "  ")}
           |  ${color("added")}: ${showSet(addedSrcs, baseIndent = "  ")}
           |  ${color("modified")}: ${showSet(modifiedSrcs, baseIndent = "  ")}
           |${color("Invalidated products")}: ${showSet(changes.removedProducts)}
           |${color("External API changes")}: ${changes.external}
           |${color("Modified binary dependencies")}: ${changes.binaryDeps}
           |${color("Initial directly invalidated classes")}: $invalidatedClasses
           |
           |${color("Sources indirectly invalidated by")}:
           |  ${color("product")}: ${showSet(byProduct, baseIndent = "  ")}
           |  ${color("binary dep")}: ${showSet(byBinaryDep, baseIndent = "  ")}
           |  ${color("external source")}: ${showSet(byExtSrcDep, baseIndent = "  ")}""".stripMargin
      )
    }

    (allInvalidatedClasses, allInvalidatedSourcefiles)
  }

  private def showSet[A](s: Set[A], baseIndent: String = ""): String = {
    if (s.isEmpty) {
      "[]"
    } else {
      s.map(baseIndent + "  " + _.toString).mkString("[\n", ",\n", "\n" + baseIndent + "]")
    }
  }

  private[this] def checkAbsolute(addedSources: List[File]): Unit =
    if (addedSources.nonEmpty) {
      addedSources.filterNot(_.isAbsolute) match {
        case first :: more =>
          val fileStrings = more match {
            case Nil      => first.toString
            case x :: Nil => s"$first and $x"
            case _        => s"$first and ${more.size} others"
          }
          sys.error(
            s"The incremental compiler requires absolute sources, but some were relative: $fileStrings")
        case Nil =>
      }
    }

  def invalidateByAllExternal(relations: Relations,
                              externalAPIChanges: APIChanges,
                              isScalaClass: String => Boolean): Set[String] = {
    (externalAPIChanges.apiChanges.flatMap { externalAPIChange =>
      invalidateByExternal(relations, externalAPIChange, isScalaClass)
    }).toSet
  }

  /** Classes invalidated by `external` classes in other projects according to the previous `relations`. */
  protected def invalidateByExternal(relations: Relations,
                                     externalAPIChange: APIChange,
                                     isScalaClass: String => Boolean): Set[String]

  /** Intermediate invalidation step: steps after the initial invalidation, but before the final transitive invalidation. */
  def invalidateIntermediate(relations: Relations,
                             changes: APIChanges,
                             isScalaClass: String => Boolean): Set[String] = {
    invalidateClasses(relations, changes, isScalaClass)
  }

  /**
   * Invalidates inheritance dependencies, transitively.  Then, invalidates direct dependencies.  Finally, excludes initial dependencies not
   * included in a cycle with newly invalidated classes.
   */
  private def invalidateClasses(relations: Relations,
                                changes: APIChanges,
                                isScalaClass: String => Boolean): Set[String] = {
    val initial = changes.allModified.toSet
    val all = (changes.apiChanges flatMap { change =>
      invalidateClass(relations, change, isScalaClass)
    }).toSet
    includeInitialCond(initial, all, allDeps(relations))
  }

  protected def allDeps(relations: Relations): (String) => Set[String]

  protected def invalidateClass(relations: Relations,
                                change: APIChange,
                                isScalaClass: String => Boolean): Set[String]

  /**
   * Conditionally include initial classes that are dependencies of newly invalidated classes.
   * Initial classes included in this step can be because of a cycle, but not always.
   */
  private[this] def includeInitialCond(initial: Set[String],
                                       currentInvalidations: Set[String],
                                       allDeps: String => Set[String]): Set[String] = {
    val newInv = currentInvalidations -- initial
    log.debug("New invalidations:\n\t" + newInv)
    val transitiveOfNew = transitiveDeps(newInv)(allDeps)
    val initialDependsOnNew = transitiveOfNew & initial
    log.debug(
      "Previously invalidated, but (transitively) depend on new invalidations:\n\t" + initialDependsOnNew)
    newInv ++ initialDependsOnNew
  }

  def externalBinaryModified(
      lookup: Lookup,
      previous: Stamps,
      current: ReadStamps,
      previousRelations: Relations)(implicit equivS: Equiv[XStamp]): File => Boolean =
    dependsOn => {
      def inv(reason: String): Boolean = {
        log.debug("Invalidating " + dependsOn + ": " + reason)
        true
      }
      def entryModified(className: String, classpathEntry: File): Boolean = {
        val resolved = Locate.resolve(classpathEntry, className)
        if (resolved.getCanonicalPath != dependsOn.getCanonicalPath)
          inv("class " + className + " now provided by " + resolved.getCanonicalPath)
        else
          fileModified(dependsOn, resolved)
      }
      def fileModified(previousFile: File, currentFile: File): Boolean = {
        val previousStamp = previous.binary(previousFile)
        val currentStamp = current.binary(currentFile)
        if (equivS.equiv(previousStamp, currentStamp))
          false
        else
          inv("stamp changed from " + previousStamp + " to " + currentStamp)
      }
      def dependencyModified(file: File): Boolean = {
        val classNames = previousRelations.libraryClassNames(file)
        classNames exists { binaryClassName =>
          // classpath has not changed since the last compilation, so use the faster detection.
          if (lookup.changedClasspathHash.isEmpty)
            lookup.lookupAnalysis(binaryClassName) match {
              case None    => false
              case Some(_) => inv(s"shadowing is detected for class $binaryClassName")
            } else
            lookup.lookupOnClasspath(binaryClassName) match {
              case None    => inv(s"could not find class $binaryClassName on the classpath.")
              case Some(e) => entryModified(binaryClassName, e)
            }
        }
      }
      (if (skipClasspathLookup) fileModified(dependsOn, dependsOn)
       else dependencyModified(dependsOn))
    }

  def currentExternalAPI(lookup: Lookup): String => AnalyzedClass = { binaryClassName =>
    {
      orEmpty(
        for {
          analysis0 <- lookup.lookupAnalysis(binaryClassName)
          analysis = analysis0 match { case a: Analysis => a }
          className <- analysis.relations.productClassName.reverse(binaryClassName).headOption
        } yield analysis.apis.internalAPI(className)
      )
    }
  }

  def orEmpty(o: Option[AnalyzedClass]): AnalyzedClass = o getOrElse APIs.emptyAnalyzedClass
  def orTrue(o: Option[Boolean]): Boolean = o getOrElse true

  protected def transitiveDeps[T](nodes: Iterable[T], logging: Boolean = true)(
      dependencies: T => Iterable[T]): Set[T] = {
    val xs = new collection.mutable.HashSet[T]
    def all(from: T, tos: Iterable[T]): Unit = tos.foreach(to => visit(from, to))
    def visit(from: T, to: T): Unit =
      if (!xs.contains(to)) {
        if (logging)
          log.debug(s"Including $to by $from")
        xs += to
        all(to, dependencies(to))
      }
    if (logging)
      log.debug("Initial set of included nodes: " + nodes)
    nodes foreach { start =>
      xs += start
      all(start, dependencies(start))
    }
    xs.toSet
  }

  private[this] def debugOuterSection(header: String): Unit = {
    import Console._
    log.debug(s"$GREEN*************************** $header$RESET")
  }

  private[this] def debugInnerSection(header: String)(content: => Any): Unit = {
    import Console._
    debug(s"$CYAN************* $header:$RESET\n$content\n$CYAN************* (end of $header)$RESET")
  }

}
