package sbt.internal.inc

import java.io.File
import java.net.URI
import java.nio.file._
import java.util.function.Consumer
import java.util.zip.ZipFile

import sbt.io.IO

import scala.collection.mutable.ListBuffer

object STJUtil {

  def withZipFs[A](uri: URI, create: Boolean = false)(action: FileSystem => A): A = {
    val env = new java.util.HashMap[String, String]
    if (create) env.put("create", "true")
    synchronized {
      val fs = FileSystems.newFileSystem(uri, env)
      try action(fs)
      finally {
        retry(fs.close())
      }
    }
  }

  def retry[A](f: => A): A = {
    try f
    catch {
      case _: FileSystemException =>
        retry(f)
    }
  }

  def withZipFs[A](file: File)(action: FileSystem => A): A = {
    withZipFs(fileToJarUri(file))(action)
  }

  def mergeJars(into: File, from: File): Unit = {
    withZipFs(into) { intoFs =>
      withZipFs(from) { fromFs =>
        Files
          .walk(fromFs.getPath("/"))
          .forEachOrdered(new Consumer[Path] {
            override def accept(t: Path): Unit = {
              if (Files.isDirectory(t)) {
                Files.createDirectories(intoFs.getPath(t.toString))
              } else {
                Files.copy(t,
                           intoFs.getPath(t.toString),
                           StandardCopyOption.COPY_ATTRIBUTES,
                           StandardCopyOption.REPLACE_EXISTING)
              }
            }
          })
      }
    }
    from.delete()
  }

  def pause(s: String): Unit = {
//    scala.io.StdIn.readLine(s)
    println(s)
  }

  // only for debugging
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

  def isWindows = System.getProperty("os.name").toLowerCase.contains("win")

  def init(jar: File, cls: String): String = jar + "!" + cls.replace("\\", "/")

  def fromJarUriAndFile(u: URI, f: String): String = {
    val jar = {
      val j = u.toString.stripPrefix("jar:file:")
      new File(if (isWindows) j.stripPrefix("/").replace("/", "\\") else j)
    }

    init(jar, f.stripPrefix("/"))
  }

  def toJarUriAndFile(s: String): (URI, String) = {
    val Array(jar0, cls) = s.split("!")
    val uri = rawPathToJarUri(jar0)
    val path = "/" + cls
    (uri, path)
  }

  def existsInJar(s: String): Boolean = {
    val (uri, cls) = toJarUriAndFile(s)
    try {
      STJUtil.withZipFs(uri) { fs: FileSystem =>
        Files.exists(fs.getPath(cls))
      }
    } catch {
      case _: FileSystemNotFoundException =>
        false
    }
  }

  def rawIdToJarFile(s: String): File = {
    val Array(jar, _) = s.toString.split("!")
    new File(jar)
  }

  def fileToJarUri(file: File): URI = {
    rawPathToJarUri(file.toString)
  }

  def jarUriToFile(jar: URI): File = {
    val x = jar.toString.stripPrefix("jar:file:")
    val r = if (isWindows) x.stripPrefix("/").replace("/", "\\") else x
    new File(r)
  }

  private def rawPathToJarUri(jar0: String) = {
    val jar = if (isWindows) "/" + jar0.replace("\\", "/") else jar0
    URI.create("jar:file:" + jar)
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
      case Array(jar, cls) if jar.endsWith(".jar") => true
      case _                                       => false
    }
  }

  private def readModifiedTimeFromJar(s: String): Long = {
    val (jar, cls) = toJarAndFile(s)
    if (jar.exists()) {
      val file = new ZipFile(jar, ZipFile.OPEN_READ)
      val time = Option(file.getEntry(cls)).map(_.getLastModifiedTime.toMillis).getOrElse(0L)
      file.close()
      time
    } else 0
  }

  def toJarAndFile(s: String): (File, String) = {
    val Array(jar, file) = s.split("!")
    (new File(jar), file)
  }

  def existsInJar(s: String): Boolean = {
    val (jar, cls) = toJarAndFile(s)
    val file = new ZipFile(jar, ZipFile.OPEN_READ)
    val exists = file.getEntry(cls) != null
    file.close()
    exists
  }

}
