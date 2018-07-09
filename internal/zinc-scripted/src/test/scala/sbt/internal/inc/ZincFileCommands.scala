package sbt.internal.inc

import java.io.File
import java.net.{ URL, URI }
import java.nio.file.{ Files, FileSystem }

import sbt.internal.scripted.FileCommands
import sbt.io.IO

class ZincFileCommands(baseDirectory: File) extends FileCommands(baseDirectory) {
  override def commandMap: Map[String, List[String] => Unit] = {
    super.commandMap + {
      "pause" noArg {
        // Redefine pause not to use `System.console`, which is too restrictive
        println(s"Pausing in $baseDirectory Press enter to continue.")
        scala.io.StdIn.readLine()
        println("Restarting the execution.")
      }
    }
  }

  override def absent(paths: List[String]): Unit = {
    val (jared, regular) = paths.partition(_.startsWith("jar:file"))
    val jaredPresent = jared.exists(existsInJar)
    val present = fromStrings(regular).filter(_.exists)
    if (present.nonEmpty || jaredPresent)
      scriptError("File(s) existed: " + present.mkString("[ ", " , ", " ]"))
  }

  override def newer(a: String, b: String): Unit = {
    val pathA = fromString(a)
    val pathB = fromString(b)
    val isNewer = pathA.exists &&
      (!pathB.exists || IO.getModifiedTimeOrZero(pathA) > IO.getModifiedTimeOrZero(pathB))
    if (!isNewer) {
      scriptError(s"$pathA is not newer than $pathB")
    }
  }

  override def exists(paths: List[String]): Unit = {
    val (jars, regular) = paths.partition(_.startsWith("jar:file"))
    val jaredNotPresent = !jars.forall(existsInJar)
    val notPresent = fromStrings(regular).filter(!_.exists)
    if (notPresent.nonEmpty || jaredNotPresent)
      scriptError("File(s) did not exist: " + notPresent.mkString("[ ", " , ", " ]"))
  }

  def existsInJar(url: String): Boolean = {
    val Array(jarPath, filePath) = url.stripPrefix("jar:file").split("!")
    val absJarPath = new File(baseDirectory, jarPath).getPath
    val absoluteJarUri = s"jar:file$absJarPath"

    STJUtil.withZipFs(URI.create(absoluteJarUri)) { fs =>
      Files.exists(fs.getPath(filePath))
    }
  }

}
