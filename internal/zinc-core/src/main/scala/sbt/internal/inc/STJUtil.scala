package sbt.internal.inc

import java.io.File
import java.net.URI
import java.nio.file.{ FileSystems, FileSystem, Files, StandardOpenOption }

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

  def mergeJars(into: File, from: File) = {
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

}
