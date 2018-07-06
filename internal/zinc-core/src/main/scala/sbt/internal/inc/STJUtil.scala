package sbt.internal.inc

import java.net.URI
import java.nio.file.{ FileSystems, FileSystem }

object STJUtil {

  def withZipFs[A](uri: URI)(action: FileSystem => A): A = {
    val env = new java.util.HashMap[String, String]
    synchronized {
      val fs = FileSystems.newFileSystem(uri, env)
      try action(fs)
      finally fs.close()
    }
  }

}
