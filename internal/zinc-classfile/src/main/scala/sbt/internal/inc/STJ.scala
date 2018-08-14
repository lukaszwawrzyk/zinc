package sbt.internal.inc

import java.net.URI

import sbt.io.IO
import java.util.zip.ZipFile
import java.io.File

import java.nio.file._
import java.util.UUID
import scala.collection.JavaConverters._

import sbt.io.IO.FileScheme
import sbt.io.syntax.URL
import xsbti.compile.{ Output, SingleOutput }

object STJ extends PathFunctions with Debugging with ForTestCode {

  def stashIndex(jar: File): IndexBasedZipFsOps.CentralDir = {
    IndexBasedZipFsOps.readCentralDir(jar)
  }

  def unstashIndex(jar: File, index: IndexBasedZipFsOps.CentralDir): Unit = {
    IndexBasedZipFsOps.writeCentralDir(jar, index)
  }

  def includeInJar(jar: File, files: Seq[(File, RelClass)]): Unit = {
    IndexBasedZipFsOps.includeInJar(jar, files)
  }

  // puts all files in `from` (overriding the original files in case of conflicts)
  // into `to`, removing `from`. In other words it merges `from` into `into`.
  def mergeJars(into: File, from: File): Unit = {
    IndexBasedZipFsOps.mergeArchives(into, from)
  }

  def createCachedStampReader(): File => Long = {
    val reader = new IndexBasedZipFsOps.CachedStampReader
    file: File =>
      if (isJar(file)) {
        val (jar, cls) = toJarAndRelClass(file.toString)
        reader.readStamp(jar, cls)
      } else {
        IO.getModifiedTimeOrZero(file)
      }
  }

  def removeFromJar(jarFile: File, classes: Iterable[RelClass]): Unit = {
    if (jarFile.exists()) {
      IndexBasedZipFsOps.removeEntries(jarFile, classes)
    }
  }

  def withPreviousJar[A](output: Output)(compile: /*extra classpath: */ Seq[File] => A): A = {
    getOutputJar(output)
      .map { outputJar =>
        val prevJarName = outputJar.getName.replace(".jar", "_prev.jar")
        val prevJar = outputJar.toPath.resolveSibling(prevJarName).toFile
        if (outputJar.exists()) {
          IO.move(outputJar, prevJar)
        }

        val result = try {
          compile(Seq(prevJar))
        } catch {
          case e: Exception =>
            if (prevJar.exists()) {
              IO.move(prevJar, outputJar)
            }
            throw e
        }

        if (prevJar.exists() && outputJar.exists()) {
          STJ.mergeJars(into = prevJar, from = outputJar)
          IO.move(prevJar, outputJar)
        }
        result
      }
      .getOrElse {
        compile(Nil)
      }
  }

}

sealed trait ForTestCode { this: PathFunctions =>

  def listFiles(jar: File): Seq[String] = {
    if (jar.exists()) {
      withZipFile(jar) { zip =>
        zip.entries().asScala.filterNot(_.isDirectory).map(_.getName).toList
      }
    } else Seq.empty
  }

  def readModifiedTimeFromJar(jc: JaredClass): Long = {
    val (jar, cls) = toJarAndRelClass(jc)
    if (jar.exists()) {
      withZipFile(jar) { zip =>
        Option(zip.getEntry(cls)).map(_.getLastModifiedTime.toMillis).getOrElse(0)
      }
    } else 0
  }

  def existsInJar(s: JaredClass): Boolean = {
    val (jar, cls) = toJarAndRelClass(s)
    jar.exists() && {
      withZipFile(jar)(zip => zip.getEntry(cls) != null)
    }
  }

  private def withZipFile[A](zip: File)(f: ZipFile => A): A = {
    val file = new ZipFile(zip)
    val result = try f(file)
    finally file.close()
    result
  }
}

sealed trait PathFunctions {

  type JaredClass = String
  type RelClass = String

  def init(jar: File, cls: RelClass): JaredClass = {
    val relClass = if (File.separatorChar == '/') cls else cls.replace(File.separatorChar, '/')
    s"$jar!$relClass"
  }

  def fromUrl(url: URL): JaredClass = {
    val Array(jarUri, cls) = url.toString.split("!/")
    val fileUri = URI.create(jarUri.stripPrefix("jar:"))
    val jar = uriToFile(fileUri)
    init(jar, cls)
  }

  // From sbt.io.IO, correctly handles URI like: "file:<any windows path>"
  private def uriToFile(uri: URI): File = {
    val part = uri.getSchemeSpecificPart
    // scheme might be omitted for relative URI reference.
    assert(
      Option(uri.getScheme) match {
        case None | Some(FileScheme) => true
        case _                       => false
      },
      s"Expected protocol to be '$FileScheme' or empty in URI $uri"
    )
    Option(uri.getAuthority) match {
      case None if part startsWith "/" => new File(uri)
      case _                           =>
        // https://github.com/sbt/sbt/issues/564
        // https://github.com/sbt/sbt/issues/3086
        // http://blogs.msdn.com/b/ie/archive/2006/12/06/file-uris-in-windows.aspx
        // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=5086147
        // The specific problem here is that `uri` will have a defined authority component for UNC names like //foo/bar/some/path.jar
        // but the File constructor requires URIs with an undefined authority component.
        if (!(part startsWith "/") && (part contains ":")) new File("//" + part)
        else new File(part)
    }
  }

  def toRelClass(jc: JaredClass): RelClass = {
    val Array(_, cls) = jc.split("!")
    cls
  }

  def jaredClassToJarFile(jc: JaredClass): File = {
    val Array(jar, _) = jc.split("!")
    new File(jar)
  }

  protected def fileToJarUri(jarFile: File): URI = {
    new URI("jar:" + jarFile.toURI.toString)
  }

  def isJar(file: File): Boolean = {
    file.getPath.split("!") match {
      case Array(jar, cls @ _) => jar.endsWith(".jar")
      case _                   => false
    }
  }

  // important as within zip we have / always and in JaredClass we always have consistent / or \
  def toJarAndRelClass(c: JaredClass): (File, RelClass) = {
    val Array(jar, relClass) = c.split("!")
    // paths within jars always have forward slashes but JaredClass has system defined slashes
    // because it is often stored in File that controls the slashes
    val fixedRelClass = relClass.replace("\\", "/")
    (new File(jar), fixedRelClass)
  }

  def isEnabled(output: Output): Boolean = {
    getOutputJar(output).isDefined
  }

  def getOutputJar(output: Output): Option[File] = {
    output match {
      case s: SingleOutput =>
        val out = s.getOutputDirectory
        if (out.getName.endsWith(".jar")) {
          Some(out)
        } else None
      case _ => None
    }

  }

  private val javacOutputSuffix = "-javac-output"
  def javacOutputDir(outputJar: File): File = {
    val outJarName = outputJar.getName
    val outDirName = outJarName + javacOutputSuffix
    outputJar.toPath.resolveSibling(outDirName).toFile
  }

  def fromJavacOutputDir(file: File): Option[File] = {
    import scala.collection.JavaConverters._
    val path = file.toPath
    val javacOutputDirComponent = path.asScala.zipWithIndex.find {
      case (component, _) =>
        component.toString.endsWith(javacOutputSuffix)
    }
    javacOutputDirComponent map {
      case (component, index) =>
        val outputJarName = component.toString.stripSuffix(javacOutputSuffix)
        val basePath = Stream.iterate(path, path.getNameCount - index + 1)(_.getParent).last
        val outputJarPath = basePath.resolve(outputJarName)
        val relClass = path.subpath(index + 1, path.getNameCount)
        new File(init(outputJarPath.toFile, relClass.toString))
    }
  }

}

sealed trait Debugging { this: PathFunctions =>

  def touchOutputFile(jar: File, msg: String): Unit = {
    val output = new SingleOutput {
      override def getOutputDirectory: File = jar
    }
    touchOutputFile(output, msg)
  }

  // fails if file is currently open
  def touchOutputFile(output: Output, msg: String): Unit = {
    getOutputJar(output).foreach { jarOut =>
      if (jarOut.exists()) {
        System.out.flush()
        println("$$$ ??? " + msg)
        System.out.flush()

        val f = jarOut.toPath.resolveSibling(s"${UUID.randomUUID()}.jar")
        Files.copy(jarOut.toPath, f)
        Files.move(f, jarOut.toPath, StandardCopyOption.REPLACE_EXISTING)

        System.out.flush()
        println(s"$$$$$$ !!! $msg")
      }
    }
  }

  // useful for debuging files
  def pause(msg: String): Unit = {
    Console.readLine(msg)
  }

}
