package sbt.internal.inc

import java.io.File
import java.net.URI
import java.nio.file._
import java.util.function.Consumer

import sbt.io.IO

import scala.collection.mutable.ListBuffer

object STJUtil {

  def withZipFs[A](uri: URI, create: Boolean = false)(action: FileSystem => A): A = {
    val env = new java.util.HashMap[String, String]
    if (create) env.put("create", "true")
    synchronized {
      val fs = FileSystems.newFileSystem(uri, env)
      try action(fs)
      finally fs.close()
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

  private val prefix = "jar#"

  def init(jar: File, cls: String): String = prefix + jar + "!" + cls.replace("\\", "/")

  def fromJarUriAndFile(u: URI, f: String): String = {
    val jar = {
      val j = u.toString.stripPrefix("jar:file:")
      new File(if (isWindows) j.stripPrefix("/").replace("/", "\\") else j)
    }

    init(jar, f.stripPrefix("/"))
  }

  def toJarUriAndFile(s: String): (URI, String) = {
    val Array(jar0, cls) = s.stripPrefix(prefix).split("!")
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
    new File(jar.stripPrefix(prefix))
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
    file.getPath.startsWith(prefix)
  }

  private def readModifiedTimeFromJar(s: String): Long = {
    try {
      val (uri, file) = toJarUriAndFile(s)
      withZipFs(uri) { fs =>
        Files.getLastModifiedTime(fs.getPath(file)).toMillis
      }
    } catch {
      case _: FileSystemNotFoundException => 0
    }
  }

}
