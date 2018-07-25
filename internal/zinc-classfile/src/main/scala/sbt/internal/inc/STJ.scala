package sbt.internal.inc

import java.net.URI

import sbt.io.IO
import java.util.zip.ZipFile
import java.io.File

import scala.collection.mutable.ListBuffer
import java.util.function.Consumer
import java.nio.file._

import scala.util.Try
import java.util.UUID

import sbt.io.IO.FileScheme
import sbt.io.syntax.URL
import xsbti.compile.{ Output, SingleOutput }

object STJ {

  def withZipFs[A](uri: URI, create: Boolean)(action: FileSystem => A): A = {
    val env = new java.util.HashMap[String, String]
    if (create) env.put("create", "true")
    val fs = FileSystems.newFileSystem(uri, env)
    try action(fs)
    finally {
      fs.close()
    }
  }

  def withZipFs[A](file: File, create: Boolean = false)(action: FileSystem => A): A = {
    withZipFs(fileToJarUri(file), create)(action)
  }

  // puts all files in `from` (overriding the original files in case of conflicts)
  // into `to`, removing `from`. In other words it merges `from` into `into`.
  def mergeJars(into: File, from: File): Unit = {
    Zip4jZipOps.mergeArchives(target = into.toPath, source = from.toPath)
  }

  // useful for debuging files
  def pause(msg: String): Unit = {
    Console.readLine(msg)
  }

  def listFiles(f: File): Seq[String] = {
    if (f.exists()) {
      withZipFs(f) { fs =>
        val list = new ListBuffer[Path]
        Files
          .walk(fs.getPath("/"))
          .forEachOrdered(new Consumer[Path] {
            override def accept(t: Path): Unit = list += t
          })
        list.map(_.toString)
      }
    } else Nil
  }

  type JaredClass = String
  type RelClass = String

  lazy val isWindows: Boolean = System.getProperty("os.name").toLowerCase.contains("win")

  def init(jar: File, cls: RelClass): JaredClass = {
    // to ensure consistent 'slashing' as those identifies are used e.g. in maps
    val classPath = if (isWindows) cls.replace("/", "\\") else cls
    s"$jar!$classPath"
  }

  // from url like returned from class loader
  def fromUrl(url: URL): JaredClass = {
    val Array(jarUri, cls) = url.toString.split("!")
    fromJarUriAndRelClass(URI.create(jarUri), cls)
  }

  def fromJarUriAndRelClass(jarUri: URI, cls: RelClass): JaredClass = {
    val jar = jarUriToFile(jarUri)
    val relClass = cls.stripPrefix("/").stripPrefix("\\")
    init(jar, relClass)
  }

  // From sbt.io.IO, correctly handles uri like: file:<a windows path>
  private[this] def uriToFile(uri: URI): File = {
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

  // this trailing slash in RelPath is messed up
  // I am not sure why I put trailing backslash here
  def toJarUriAndRelClass(jc: JaredClass): (URI, RelClass) = {
    val Array(jar, cls) = jc.split("!")
    val uri = fileToJarUri(new File(jar))
    val path = (if (isWindows) "\\" else "/") + cls
    (uri, path)
  }

  def jaredClassToJarFile(jc: JaredClass): File = {
    val Array(jar, _) = jc.split("!")
    new File(jar)
  }

  def jarUriToFile(jarUri: URI): File = {
    val fileUri = URI.create(jarUri.toString.stripPrefix("jar:"))
    uriToFile(fileUri)
  }

  def fileToJarUri(jarFile: File): URI = {
    new URI("jar:" + jarFile.toURI.toString)
  }

  def getModifiedTimeOrZero(file: File): Long = {
    if (file.exists()) {
      IO.getModifiedTimeOrZero(file)
    } else if (isJar(file)) {
      readModifiedTimeFromJar(file.getPath)
    } else {
      0
    }
  }

  def isJar(file: File): Boolean = {
    file.getPath.split("!") match {
      case Array(jar, cls) => jar.endsWith(".jar")
      case _               => false
    }
  }

  def readModifiedTimeFromJar(jc: JaredClass): Long = {
    val (jar, cls) = toJarAndRelClass(jc)
    if (jar.exists()) {
      val file = new ZipFile(jar, ZipFile.OPEN_READ)
      val time = Option(file.getEntry(cls)).map(_.getLastModifiedTime.toMillis).getOrElse(0L)
      file.close()
      time
    } else 0
  }

  def toJarAndRelClass(c: JaredClass): (File, RelClass) = {
    val Array(jar, relClass) = c.split("!")
    // paths within jars always have forward slashes but JaredClass has system defined slashes
    // because it is often stored in File that controls the slashes
    val fixedRelClass = relClass.replace("\\", "/")
    (new File(jar), fixedRelClass)
  }

  def existsInJar(s: JaredClass): Boolean = {
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

  def withPreviousJar[A](output: Output)(compile: /*extra cp: */ Seq[File] => A): A = {
    extractJarOutput(output)
      .map { outputJar =>
        // cleanup stuff from other compilations
        Option(outputJar.toPath.getParent.toFile.listFiles()).foreach { files =>
          files
            .filter(f => isTmpJar(f))
            .foreach(f => Try(f.delete()))
        }

        val prevJar =
          outputJar.toPath.resolveSibling(outputJar.getName.replace(".jar", "_prev.jar")).toFile
        if (outputJar.exists()) {
          IO.move(outputJar, prevJar)
        }

        val res = try compile(Seq(prevJar))
        catch {
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
        res
      }
      .getOrElse {
        compile(Nil)
      }
  }

  def removeFromJar(jar: URI, classes: Iterable[RelClass]): Unit = {
    val jarFile = jarUriToFile(jar)
    if (jarFile.exists()) {
      withZipFs(jarFile) { fs =>
        classes.foreach { cls =>
          Files.delete(fs.getPath(cls))
        }
      }
    }
  }

  def extractJarOutput(output: Output): Option[File] = {
    output match {
      case s: SingleOutput =>
        val out = s.getOutputDirectory
        if (out.getName.endsWith(".jar")) {
          Some(out)
        } else None
      case _ => None
    }

  }

  // for debugging purposes, fails if file is currently open
  def touchOutputFile(output: Output, msg: String): Unit = {
    STJ.extractJarOutput(output).foreach { jarOut =>
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

  val tmpJarPrefix = "tmpjar"
  def tmpJar(file: File, id: String): File = {
    file.toPath.resolveSibling(s"$tmpJarPrefix$id${UUID.randomUUID()}.jar").toFile
  }

  private def isTmpJar[A](f: File) = {
    f.getName.endsWith(".jar") && f.getName.startsWith(tmpJarPrefix)
  }

}
