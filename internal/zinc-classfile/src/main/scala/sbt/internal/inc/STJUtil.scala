package sbt.internal.inc

import java.net.{ URI, URL }

import sbt.io.IO
import java.util.zip.ZipFile
import java.io.File

import scala.collection.mutable.ListBuffer
import java.util.function.Consumer
import java.nio.file._

import scala.util.{ Random, Try }
import java.util.UUID

import sbt.io.IO.{ toFile, FileScheme }
import sbt.io.syntax.URL
import xsbti.compile.{ Output, SingleOutput }

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

  def retry(f: => Unit): Unit = {
    try f
    catch {
      case _: FileSystemException =>
        Thread.sleep(200)
        println(
          s"~@@@@@@@@~ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! RETRY IN PLACE ${Random.nextInt}")
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
//    forcePause(s)
    println(s)
  }

  def forcePause(s: String): Unit = {
    scala.io.StdIn.readLine(s)
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

  def isWindows: Boolean = System.getProperty("os.name").toLowerCase.contains("win")

  def init(jar: File, cls: String): String =
    jar + "!" + (if (isWindows) cls.replace("/", "\\") else cls)

  def fromUrl(u: URL): String = {
    val Array(jarUri, cls) = u.toString.split("!")
    fromJarUriAndFile(URI.create(jarUri), cls)
  }

  def fromJarUriAndFile(u: URI, f: String): String = {
    val jar = uriToFile(URI.create(u.toString.stripPrefix("jar:")))
    init(jar, f.stripPrefix("/").stripPrefix("\\"))
  }

  // From sbt.io.IO, correctly handles uri like: file:<even a windows path>
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

  def toJarUriAndFile(s: String): (URI, String) = {
    val Array(jar0, cls) = s.split("!")
    val uri = rawPathToJarUri(jar0)
    val path = (if (isWindows) "\\" else "/") + cls
    (uri, path)
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
      case Array(jar, cls) => jar.endsWith(".jar")
      case _               => false
    }
  }

  private def readModifiedTimeFromJar(s: String): Long = {
    val (jar, cls) = toJarAndFile(s)
    if (jar.exists()) {
      // OPENS OUTPUT.JAR !!! (when collecting initial stamps)
      val file = new ZipFile(jar, ZipFile.OPEN_READ)
      val time = Option(file.getEntry(cls)).map(_.getLastModifiedTime.toMillis).getOrElse(0L)
      file.close()
      time
    } else 0
  }

  def toJarAndFile(s: String): (File, String) = {
    val Array(jar, file) = s.split("!")
    (new File(jar), file.replace("\\", "/"))
  }

  def existsInJar(s: String): Boolean = {
    val (jar, cls) = toJarAndFile(s)
    if (!jar.exists()) {
      false
    } else {
      val file = new ZipFile(jar, ZipFile.OPEN_READ)
      val exists = file.getEntry(cls) != null
      file.close()
      exists
    }
  }

  def withPreviousJar[A](output: Output)(
      compile: ( /*extra cp: */ Seq[File], /*outputOverride: */ File) => A): A = {
    val outputJar = output.getSingleOutput.get

    // cleanup stuff from other compilations
    pause("Trying to get rid of tmpjars")
    Option(outputJar.toPath.getParent.toFile.listFiles()).foreach { files =>
      files
        .filter(f => f.getName.endsWith(".jar") && f.getName.startsWith("tmpjar"))
        .foreach(f => Try(f.delete()))
    }

    val prevJar = outputJar.toPath.resolveSibling("tmpjarprev" + UUID.randomUUID() + ".jar").toFile
    if (outputJar.exists()) {
      pause(s"Prev jar set as $prevJar output jar ($outputJar) exists so moving it ")
      // MOVES OUTPUT.JAR !!!
      IO.move(outputJar, prevJar)
      pause(s"Moved")
    }

    val jarForStupidScalac =
      outputJar.toPath
        .resolveSibling("tmpjarout" + UUID.randomUUID() + "_tmpjarsep_" + outputJar.getName)
        .toFile
    pause(s"About to run compilation, will enforce $jarForStupidScalac as output")
    val res = try compile(Seq(prevJar), jarForStupidScalac)
    catch {
      case e: Exception =>
        pause("Compilation failed")
        if (prevJar.exists()) {
          pause(s"Reverting prev jar $prevJar onto $outputJar")
          // MOVES TO OUTPUT.JAR !!!
          IO.move(prevJar, outputJar)
          pause("Reverted prev jar")
        }
        throw e
    }

    if (prevJar.exists()) {
      if (jarForStupidScalac.exists()) {
        // most complex case: scala compilation completed to a temp jar and prev jars exists, they need to be merged
        val tmpTargetJar = prevJar.toPath.resolveSibling("~~merge~target~~.jar")
        val tmpSrcJar = jarForStupidScalac.toPath.resolveSibling("~~merge~source~~.jar")
        pause(
          s"Prev jar and temp out jar exist so merging those, will copy $prevJar on $tmpTargetJar")
        Files.copy(prevJar.toPath, tmpTargetJar)
        pause(s"Will copy $jarForStupidScalac on $tmpSrcJar")
        Files.copy(jarForStupidScalac.toPath, tmpSrcJar)
        pause(s"Prev jar and out jar exist so merging those $tmpTargetJar and $tmpSrcJar")
        STJUtil.mergeJars(into = tmpTargetJar.toFile, from = tmpSrcJar.toFile)
        pause(s"merged, moving prevJar on outJar $tmpTargetJar to $outputJar")
        // MOVES TO OUTPUT.JAR !!!
        IO.move(tmpTargetJar.toFile, outputJar)
        pause(s"moved, trying to remove $jarForStupidScalac")
        Try(Files.delete(jarForStupidScalac.toPath)) // probably will fail anyway
        pause("Finally done")
      } else {
        // Java only compilation case - probably temporary as java should go to jar as well
        pause("java path")
        // MOVES TO OUTPUT.JAR !!!
        IO.move(prevJar, outputJar)
      }
    } else {
      if (jarForStupidScalac.exists()) {
        // prev jar does not exist so it is the first compilation for scalac, just copy temp jar to output
        pause(s"Copying $jarForStupidScalac to $outputJar")
        // COPIES ONTO OUTPUT.JAR !!!
        Files.copy(jarForStupidScalac.toPath,
                   outputJar.toPath,
                   StandardCopyOption.COPY_ATTRIBUTES,
                   StandardCopyOption.REPLACE_EXISTING)
        STJUtil.touchOutputFile(output, "After creating output jar")
      } else {
        // there is no output jar, so it was a java compilation without prev jar, nothing to do
      }
    }

    res
  }

  def removeFromJar(jar: URI, classes: Iterable[String]): Unit = {
    val origFile = STJUtil.jarUriToFile(jar)
    if (origFile.exists()) {
      val tmpFile = origFile.toPath.resolveSibling("~~removing~tmp~~.jar").toFile
      Files.copy(origFile.toPath, tmpFile.toPath)
      STJUtil.withZipFs(STJUtil.fileToJarUri(tmpFile)) { fs =>
        classes.foreach { cls =>
          Files.delete(fs.getPath(cls))
        }
      }
      Files.move(tmpFile.toPath, origFile.toPath, StandardCopyOption.REPLACE_EXISTING)
    }
  }

  def extractJarOutput(output: Output): Option[File] = {
    output match {
      case s: SingleOutput =>
        val out = s.getSingleOutput.get
        if (out.getName.endsWith(".jar")) {
          Some(out)
        } else None
      case _ => None
    }

  }

  def touchOutputFile(output: File, msg: String): Unit = {
    System.out.flush()
    println("$$$ ??? " + msg)
    System.out.flush()

    val f = output.toPath.resolveSibling(s"${UUID.randomUUID()}.jar")
    Files.copy(output.toPath, f)
    Files.move(f, output.toPath, StandardCopyOption.REPLACE_EXISTING)

    System.out.flush()
    println(s"$$$$$$ !!! $msg")
    System.out.flush()
  }

  def touchOutputFile(output: Output, msg: String): Unit = {
    STJUtil.extractJarOutput(output).foreach { jarOut =>
      if (jarOut.exists()) {
        STJUtil.touchOutputFile(jarOut, msg)
      }
    }
  }

  def outputMustNotExist(output: Output): Unit = {
    STJUtil.extractJarOutput(output).foreach { jarOut =>
      if (jarOut.exists()) {
        println(s"$jarOut exists :/")
        throw new Exception(s"$jarOut exists :/")
      } else {
        println(s"$jarOut not exists; is ok ;D")
      }
    }
  }

}
