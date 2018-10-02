package sbt.internal.inc

import sbt.io.IO
import java.util.zip.ZipFile
import java.io.File
import java.util.UUID

import scala.collection.JavaConverters._
import sbt.io.syntax.URL
import xsbti.AnalysisCallback
import xsbti.compile.{ Output, SingleOutput }
import sbt.util.InterfaceUtil.toOption

/**
 * This is a utility class that provides a set of functions that
 * are used to implement straight to jar compilation.
 *
 *  [[xsbt.JarUtils]] is a class that has similar purpose and
 *  duplicates some of the code, as it is difficult to share it.
 */
object JarUtils {

  /** Represents a path to a class file located inside a jar, relative to this jar */
  type RelClass = String

  /** `ClassInJar` is an identifier for a class located inside a jar.
   * For plain class files it is enough to simply use the actual file
   * system path. A class in a jar is identified as a path to the jar
   * and path to the class within that jar (`RelClass`). Those two values
   * are held in one string separated by `!`. Slashes in both
   * paths are consistent with `File.separatorChar` as the actual
   * string is usually kept in `File` object.
   *
   * As an example: given a jar file "C:\develop\zinc\target\output.jar"
   * and a relative path to the class "sbt/internal/inc/Compile.class"
   * The resulting identifier would be:
   * "C:\develop\zinc\target\output.jar!sbt\internal\inc\Compile.class"
   */
  class ClassInJar(override val toString: String) extends AnyVal {

    def relClass: RelClass = toJarAndRelClass._2

    def toJarAndRelClass: (File, RelClass) = {
      val Array(jar, cls) = toString.split("!")
      // ClassInJar stores RelClass part with File.separatorChar, however actual paths in zips always use '/'
      val relClass = cls.replace('\\', '/')
      (new File(jar), relClass)
    }

    /**
     * Wraps the string value inside a [[java.io.File]] object.
     * File is needed to e.g. be compatible with [[xsbti.compile.analysis.ReadStamps]] interface.
     */
    def toFile: File = new File(toString)

  }

  object ClassInJar {

    /**
     * The base constructor for `ClassInJar`
     * @param jar the jar file
     * @param cls the relative path to class within the jar
     * @return a proper ClassInJar identified by given jar and path to class
     */
    def apply(jar: File, cls: RelClass): ClassInJar = {
      // This identifier will be stored as a java.io.File. Its constructor will normalize slashes
      // which means that the identifier to be consistent should at all points have consistent
      // slashes for safe comparisons, especially in sets or maps.
      val relClass = if (File.separatorChar == '/') cls else cls.replace('/', File.separatorChar)
      new ClassInJar(s"$jar!$relClass")
    }

    /**
     * Converts an URL to a class in a jar to `ClassInJar`. The method is rather trivial
     * as it also takes precomputed path to the jar that it logically should extract itself.
     * However as it is computed at the callsite anyway, to avoid recomputation it is passed
     * as a parameter/
     *
     * As an example, given a URL:
     * "jar:file:///C:/develop/zinc/target/output.jar!/sbt/internal/inc/Compile.class"
     * and a file: "C:\develop\zinc\target\output.jar"
     * it will create a `ClassInJar` represented as:
     * "C:\develop\zinc\target\output.jar!sbt\internal\inc\Compile.class"
     *
     * @param url url to a class inside a jar
     * @param jar a jar file where the class is located in
     * @return the class inside a jar represented as `ClassInJar`
     */
    def fromURL(url: URL, jar: File): ClassInJar = {
      val Array(_, cls) = url.getPath.split("!/")
      apply(jar, cls)
    }

    /** Initialized `ClassInJar` based on its serialized value stored inside a file */
    def fromFile(f: File): ClassInJar = new ClassInJar(f.toString)
  }

  /**
   * Options that have to be specified when running scalac in order
   * for Straight to Jar to work properly.
   *
   * -YdisableFlatCpCaching is needed to disable caching the output jar
   * that changes between compilation runs (incremental compilation cycles).
   * Caching may hide those changes and lead into incorrect results.
   */
  val scalacOptions = Set("-YdisableFlatCpCaching")

  /**
   * Options that have to be specified when running javac in order
   * for Straight to Jar to work properly.
   *
   * -XDuseOptimizedZip=false holds jars open that causes problems
   * with locks on Windows.
   */
  val javacOptions = Set("-XDuseOptimizedZip=false")

  /** Reads current index of a jar file to allow restoring it later with `unstashIndex` */
  def stashIndex(jar: File): IndexBasedZipFsOps.CentralDir = {
    IndexBasedZipFsOps.readCentralDir(jar)
  }

  /** Replaces index in given jar file with specified one */
  def unstashIndex(jar: File, index: IndexBasedZipFsOps.CentralDir): Unit = {
    IndexBasedZipFsOps.writeCentralDir(jar, index)
  }

  /**
   * Adds plain files to specified jar file. See [[sbt.internal.inc.IndexBasedZipOps#includeInArchive]] for details.
   */
  def includeInJar(jar: File, files: Seq[(File, RelClass)]): Unit = {
    IndexBasedZipFsOps.includeInArchive(jar, files)
  }

  /**
   * Merges contents of two jars. See [[sbt.internal.inc.IndexBasedZipOps#mergeArchives]] for details.
   */
  def mergeJars(into: File, from: File): Unit = {
    IndexBasedZipFsOps.mergeArchives(into, from)
  }

  /**
   * Reads all timestamps from given jar file. Returns a function that
   * allows to access them by `ClassInJar` wrapped in `File`.
   */
  def readStamps(jar: File): File => Long = {
    val stamps = new IndexBasedZipFsOps.CachedStamps(jar)
    file =>
      stamps.getStamp(ClassInJar.fromFile(file).relClass)
  }

  /**
   * Removes specified entries from a jar file.
   */
  def removeFromJar(jarFile: File, classes: Iterable[RelClass]): Unit = {
    if (jarFile.exists()) {
      IndexBasedZipFsOps.removeEntries(jarFile, classes)
    }
  }

  /**
   * Runs the compilation with previous jar if required.
   *
   * When compiling directly to a jar, scalac will produce
   * a jar file, if one exists it will be overwritten rather
   * than updated. For sake of incremental compilation it
   * is required to merge the output from previous compilation(s)
   * and the current one. To make it work, the jar output from
   * previous compilation is stored aside (renamed) to avoid
   * overwrite. The compilation is run normally to the specified
   * output jar. The produced output jar is then merged with
   * jar from previous compilation(s).
   *
   * Classes from previous jar need to be available for the current
   * compiler run - they need to be added to the classpath. This is
   * implemented by taking a function that given additional classpath
   * runs the compilation.
   *
   * If compilation fails, it does not produce a jar, the previous jar
   * is simply reverted (moved to output jar path).
   *
   * If the previous output does not exist or the output is not a jar
   * at all (JarUtils feature is disabled) this function runs a normal
   * compilation.
   *
   * @param output output for scalac compilation
   * @param callback analysis callback used to set previus jar
   * @param compile function that given extra classpath for compiler runs the compilation
   */
  def withPreviousJar[A](output: Output)(compile: /*extra classpath: */ Seq[File] => A): A = {
    getOutputJar(output).filter(_.exists()) match {
      case Some(outputJar) =>
        val prevJar = createPrevJarPath()
        IO.move(outputJar, prevJar)

        val result = try {
          compile(Seq(prevJar))
        } catch {
          case e: Exception =>
            IO.move(prevJar, outputJar)
            throw e
        }

        if (outputJar.exists()) {
          JarUtils.mergeJars(into = prevJar, from = outputJar)
        }
        IO.move(prevJar, outputJar)
        result
      case None =>
        compile(Nil)
    }
  }

  private def createPrevJarPath(): File = {
    val tempDir =
      sys.props.get("zinc.compile-to-jar.tmp-dir").map(new File(_)).getOrElse(IO.temporaryDirectory)
    val prevJarName = s"$prevJarPrefix-${UUID.randomUUID()}.jar"
    tempDir.toPath.resolve(prevJarName).toFile
  }

  val prevJarPrefix: String = "prev-jar"

  /** Checks if given file stores a ClassInJar */
  def isClassInJar(file: File): Boolean = {
    file.toString.split("!") match {
      case Array(jar, _) => jar.endsWith(".jar")
      case _             => false
    }
  }

  /**
   * Determines if Straight to Jar compilations is enabled
   * by inspecting if compilation output is a jar file
   */
  def isCompilingToJar(output: Output): Boolean = {
    getOutputJar(output).isDefined
  }

  /** Extracts a jar file from the output if it is set to be a single jar. */
  def getOutputJar(output: Output): Option[File] = {
    output match {
      case s: SingleOutput =>
        Some(s.getOutputDirectory).filter(_.getName.endsWith(".jar"))
      case _ => None
    }
  }

  /**
   * As javac does not support compiling directly to jar it is required to
   * change its output to a directory that is temporary, as after compilation
   * the plain classes are put into a zip file and merged with the output jar.
   *
   * This method returns path to this directory based on output jar. The result
   * of this method has to be deterministic as it is called from different places
   * independently.
   */
  def javacTempOutput(outputJar: File): File = {
    val outJarName = outputJar.getName
    val outDirName = outJarName + "-javac-output"
    outputJar.toPath.resolveSibling(outDirName).toFile
  }

  /** Lists class file entries in jar e.g. sbt/internal/inc/JarUtils.class */
  def listClassFiles(jar: File): Seq[String] = {
//    IndexBasedZipFsOps.listEntries(jar).filter(_.endsWith(".class"))
    withZipFile(jar) { zip =>
      zip
        .entries()
        .asScala
        .filterNot(_.isDirectory)
        .map(_.getName)
        .filter(_.endsWith(".class"))
        .toList
    }
  }

  object OutputJarContent {

    type FQN = String

    private var content: Option[Content] = None
    private var updateCacheOnAccess: () => Unit = _

    private val readFromOutJar = () => {
      content.foreach(_.readFromOutputJar())
    }
    private val doNothing = () => ()

    def initialize(output: Output): Unit = {
      content = JarUtils.getOutputJar(output).map(new Content(_))
      updateCacheOnAccess = readFromOutJar
      println("Calling update on access from init")
      updateCacheOnAccess()
      updateCacheOnAccess = doNothing
    }

    def dependencyPhaseCompleted(): Unit = {
      updateCacheOnAccess = readFromOutJar
      println("Dependency phase completed")
    }

    def scalacRunCompleted(): Unit = {
      updateCacheOnAccess = doNothing
      println("Scalac run completed")
    }

    def addClasses(classes: Set[JarUtils.RelClass]): Unit = {
      content.foreach(_.add(classes.map(toFQN)))
    }

    private def toFQN(clazz: JarUtils.RelClass) = clazz.stripSuffix(".class").replace('/', '.')

    def get(): Set[FQN] = {
      println("Calling update on access from get")
      updateCacheOnAccess()
      updateCacheOnAccess = doNothing
      content.fold(Set.empty[FQN])(_.current())
    }

    class Content(outputJar: File) {
      private var content: Set[FQN] = Set.empty

      def current(): Set[FQN] = content

      def add(classes: Set[FQN]): Unit = content ++= classes

      def readFromOutputJar(): Unit = {
        println("Trying to read from out jar")
        if (outputJar.exists()) {
          content = JarUtils.listClassFiles(outputJar).map(toFQN)(collection.breakOut)
          println(s"Out jar existed, read: $content")
        }
      }

    }

  }

  /* Methods below are only used for test code. They are not optimized for performance. */
  /** Reads timestamp of given jared class */
  def readModifiedTime(jc: ClassInJar): Long = {
    val (jar, cls) = jc.toJarAndRelClass
    if (jar.exists()) {
      withZipFile(jar) { zip =>
        Option(zip.getEntry(cls)).map(_.getLastModifiedTime.toMillis).getOrElse(0)
      }
    } else 0
  }

  /** Checks if given jared class exists */
  def exists(jc: ClassInJar): Boolean = {
    val (jar, cls) = jc.toJarAndRelClass
    jar.exists() && {
      withZipFile(jar)(zip => zip.getEntry(cls) != null)
    }
  }

  private def withZipFile[A](zip: File)(f: ZipFile => A): A = {
    val file = new ZipFile(zip)
    try f(file)
    finally file.close()
  }
}
