package sbt.internal.inc

import java.nio.file._
import java.net.URI

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

}
