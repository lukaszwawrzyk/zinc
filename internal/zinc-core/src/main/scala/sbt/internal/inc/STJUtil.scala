package sbt.internal.inc

import java.io.File
import java.net.URI
import java.nio.file._
import java.util.function.Consumer

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
    withZipFs(URI.create("jar:file:" + file.getPath))(action)
  }

  def mergeJarsSh(into: File, from: File): Unit = {
    val parent = into.toPath.getParent
    val intoTmpDir = parent.resolve("intotmp")
    val fromTmpDir = parent.resolve("fromtmp")
    val script =
      s"""mkdir $intoTmpDir
         |mkdir $fromTmpDir
         |(cd $intoTmpDir; unzip $into)
         |(cd $fromTmpDir; unzip $from)
         |rsync -a $fromTmpDir/ $intoTmpDir/
         |rm -rf $into
         |jar -cvf $into -C $intoTmpDir .
         |rm -rf $intoTmpDir $fromTmpDir
      """.stripMargin
    val scriptFile = parent.resolve("script.sh")
    Files.write(scriptFile, script.getBytes, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
    import scala.sys.process._
    s"bash $scriptFile".!
    Files.deleteIfExists(scriptFile)
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

  def existsInJar(uri: String): Boolean = {
    val Array(jar, cls) = uri.split("!")
    try {
      STJUtil.withZipFs(URI.create(jar)) { fs: FileSystem =>
        Files.exists(fs.getPath(cls))
      }
    } catch {
      case _: FileSystemNotFoundException =>
        false
    }
  }

}
