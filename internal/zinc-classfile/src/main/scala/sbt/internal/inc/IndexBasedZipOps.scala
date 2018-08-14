package sbt.internal.inc

import java.nio.channels.{ FileChannel, Channels, ReadableByteChannel }
import java.io._
import java.nio.file.{ Files, Path }
import java.util.UUID
import java.util.zip.{ Deflater, ZipOutputStream, ZipEntry }

import sbt.io.{ IO, Using }

trait IndexBasedZipOps extends CreateZip {

  class CachedStampReader {
    private var cachedNameToTimestamp: Map[String, Long] = _

    def readStamp(jar: File, cls: String): Long = {
      if (cachedNameToTimestamp == null) {
        cachedNameToTimestamp = initializeCache(jar)
      }
      cachedNameToTimestamp.getOrElse(cls, 0)
    }

    private def initializeCache(jar: File): Map[String, Long] = {
      if (jar.exists()) {
        val metadata = readMetadata(jar.toPath)
        val headers = getHeaders(metadata)
        headers.map(header => getFileName(header) -> getLastModifiedTime(header))(
          collection.breakOut)
      } else {
        Map.empty
      }
    }
  }

  // TODO adjust STJ (PathFunctions) so that it is not needed
  def removeEntries(jarFile: File, classes: Iterable[String]): Unit = {
    val cleanPaths = classes.map(_.replace("\\", "/").stripPrefix("/"))
    removeEntries(jarFile.toPath, cleanPaths.toSet)
  }

  def mergeArchives(into: File, from: File): Unit = {
    mergeArchives(into.toPath, from.toPath)
  }

  def includeInJar(jar: File, files: Seq[(File, String)]): Unit = {
    if (jar.exists()) {
      val tempZip = jar.toPath.resolveSibling(s"${UUID.randomUUID()}.jar").toFile
      createZip(tempZip, files)
      mergeArchives(jar, tempZip)
    } else {
      createZip(jar, files)
    }
  }

  def readCentralDir(file: File): Metadata = {
    readMetadata(file.toPath)
  }

  def writeCentralDir(file: File, centralDir: Metadata): Unit = {
    storeMetadata(file.toPath, centralDir)
  }

  type Metadata
  type Header

  private def storeMetadata(path: Path, newMetadata: Metadata): Unit = {
    val currentMetadata = readMetadata(path)
    val currentCentralDirStart = truncateMetadata(currentMetadata, path)
    setCentralDirStart(newMetadata, currentCentralDirStart)
    finalizeZip(newMetadata, path, currentCentralDirStart)
  }

  private def removeEntries(path: Path, toRemove: Set[String]): Unit = {
    val metadata = readMetadata(path)
    removeEntriesFromCentralDir(metadata, toRemove)
    val writeOffset = truncateMetadata(metadata, path)
    finalizeZip(metadata, path, writeOffset)
  }

  private def removeEntriesFromCentralDir(metadata: Metadata, toRemove: Set[String]): Unit = {
    val headers = getHeaders(metadata)
    val clearedHeaders = headers.filterNot(header => toRemove.contains(getFileName(header)))
    setHeaders(metadata, clearedHeaders)
  }

  private def mergeArchives(target: Path, source: Path): Unit = {
    val targetMetadata = readMetadata(target)
    val sourceMetadata = readMetadata(source)

    // "source" starts where "target" ends
    val sourceStart = truncateMetadata(targetMetadata, target)
    // "source" is as long as from its beginning till the start of central dir
    val sourceLength = getCentralDirStart(sourceMetadata)

    transferAll(source, target, startPos = sourceStart, bytesToTransfer = sourceLength)

    val mergedHeaders = mergeHeaders(targetMetadata, sourceMetadata, sourceStart)
    setHeaders(targetMetadata, mergedHeaders)

    val centralDirStart = sourceStart + sourceLength
    setCentralDirStart(targetMetadata, centralDirStart)

    finalizeZip(targetMetadata, target, centralDirStart)

    Files.delete(source)
  }

  private def mergeHeaders(
      targetModel: Metadata,
      sourceModel: Metadata,
      sourceStart: Long
  ): Seq[Header] = {
    val sourceHeaders = getHeaders(sourceModel)
    sourceHeaders.foreach { header =>
      // potentially offsets should be updated for each header
      // not only in central directory but a valid zip tool
      // should not rely on that unless the file is corrupted
      val currentOffset = getFileOffset(header)
      val newOffset = currentOffset + sourceStart
      setFileOffset(header, newOffset)
    }

    // override files from target with files from source
    val sourceNames = sourceHeaders.map(getFileName).toSet
    val targetHeaders = getHeaders(targetModel).filterNot(h => sourceNames.contains(getFileName(h)))

    targetHeaders ++ sourceHeaders
  }

  private def truncateMetadata(metadata: Metadata, path: Path): Long = {
    val sizeAfterTruncate = getCentralDirStart(metadata)
    new FileOutputStream(path.toFile, true).getChannel
      .truncate(sizeAfterTruncate)
      .close()
    sizeAfterTruncate
  }

  private def finalizeZip(
      metadata: Metadata,
      path: Path,
      metadataStart: Long
  ): Unit = {
    val fileOutputStream = new FileOutputStream(path.toFile, true)
    fileOutputStream.getChannel.position(metadataStart)
    val outputStream = new BufferedOutputStream(fileOutputStream)
    dumpMetadata(metadata, outputStream)
    outputStream.close()
  }

  private def transferAll(
      source: Path,
      target: Path,
      startPos: Long,
      bytesToTransfer: Long
  ): Unit = {
    val sourceFile = openFileForReading(source)
    val targetFile = openFileForWriting(target)
    var remaining = bytesToTransfer
    var offset = startPos
    while (remaining > 0) {
      val transferred =
        targetFile.transferFrom(sourceFile, /*position =*/ offset, /*count = */ remaining)
      offset += transferred
      remaining -= transferred
    }
    sourceFile.close()
    targetFile.close()
  }

  private def openFileForReading(path: Path): ReadableByteChannel = {
    Channels.newChannel(new BufferedInputStream(Files.newInputStream(path)))
  }

  private def openFileForWriting(path: Path): FileChannel = {
    new FileOutputStream(path.toFile, true).getChannel
  }

  protected def readMetadata(path: Path): Metadata

  protected def getCentralDirStart(metadata: Metadata): Long
  protected def setCentralDirStart(metadata: Metadata, centralDirStart: Long): Unit

  protected def getHeaders(metadata: Metadata): Seq[Header]
  protected def setHeaders(metadata: Metadata, headers: Seq[Header]): Unit

  protected def getFileName(header: Header): String

  protected def getFileOffset(header: Header): Long
  protected def setFileOffset(header: Header, offset: Long): Unit
  protected def getLastModifiedTime(header: Header): Long

  protected def dumpMetadata(metadata: Metadata, outputStream: OutputStream): Unit

}

trait CreateZip {

  def createZip(target: File, files: Seq[(File, String)]): Unit = {
    IO.createDirectory(target.getParentFile)
    withZipOutput(target) { output =>
      writeZip(files, output)
    }
  }

  private def withZipOutput(file: File)(f: ZipOutputStream => Unit): Unit = {
    Using.fileOutputStream()(file) { fileOut =>
      val zipOut = new ZipOutputStream(fileOut)
      zipOut.setMethod(ZipOutputStream.DEFLATED)
      zipOut.setLevel(Deflater.NO_COMPRESSION)
      try { f(zipOut) } finally { zipOut.close() }
    }
  }

  private def writeZip(files: Seq[(File, String)], output: ZipOutputStream): Unit = {
    def makeFileEntry(file: File, name: String): ZipEntry = {
      val e = new ZipEntry(name)
      e.setTime(IO.getModifiedTimeOrZero(file))
      e
    }

    def addFileEntry(file: File, name: String): Unit = {
      output.putNextEntry(makeFileEntry(file, name))
      IO.transfer(file, output)
      output.closeEntry()
    }

    files.foreach { case (file, name) => addFileEntry(file, normalizeName(name)) }
  }

  private def normalizeName(name: String): String = {
    val sep = File.separatorChar
    if (sep == '/') name else name.replace(sep, '/')
  }

}
