package sbt.internal.inc

import java.io.File
import java.net.URI
import java.nio.file._
import java.util.UUID
import java.util.function.Consumer
import java.util.zip.ZipFile

import sbt.io.IO
import xsbti.compile.{ Output, SingleOutput }

import scala.collection.mutable.ListBuffer
import scala.util.{ Random, Try }

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

  def isWindows: Boolean = System.getProperty("os.name").toLowerCase.contains("win")

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
    (new File(jar), file)
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

  // a fake datastruct to avoid altering analysis callback interface
  def toTmpAndTarget(f: File): (Option[File], File) = {
    f.toString.split("##") match {
      case Array(tmp, target) =>
        (Some(new File(tmp)), new File(target))
      case Array(target) =>
        (None, new File(target))
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

}
