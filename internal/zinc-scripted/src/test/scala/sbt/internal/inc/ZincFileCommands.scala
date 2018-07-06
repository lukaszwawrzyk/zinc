package sbt.internal.inc

import java.io.File
import java.net.URL

import sbt.internal.scripted.FileCommands

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

  override def newer(a: String, b: String): Unit = super.newer(a, b)

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
    val absoluteUrl = s"jar:file$absJarPath!$filePath"
    new URL(absoluteUrl).openConnection().getContentLength >= 0
  }
}
