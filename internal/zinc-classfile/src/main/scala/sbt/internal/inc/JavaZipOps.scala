package sbt.internal.inc

import java.util.function.Consumer
import java.nio.file._
import java.io.File
import java.net.URI

import sbt.internal.inc.STJ.{ RelClass, withZipFs }

object JavaZipOps {

  def withZipFs[A](uri: URI, create: Boolean)(action: FileSystem => A): A = {
    val env = new java.util.HashMap[String, String]
    if (create) env.put("create", "true")
    val fs = FileSystems.newFileSystem(uri, env)
    try action(fs)
    finally {
      fs.close()
    }
  }

  def removeFromJar(jarFile: File, classes: Iterable[RelClass]): Unit = {
    STJ.withZipFs(jarFile) { fs =>
      classes.foreach { cls =>
        Files.deleteIfExists(fs.getPath(cls))
      }
    }
  }

  def mergeJars(into: File, from: File): Unit = {
    STJ.withZipFs(into) { intoFs =>
      STJ.withZipFs(from) { fromFs =>
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

}
