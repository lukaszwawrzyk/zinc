package sbt.internal.inc

import java.nio.channels.{ FileChannel, Channels, ReadableByteChannel }
import java.io._
import java.nio.file.{ Files, Path }

trait IndexBasedZipOps {

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
        headers.map(header => getFileName(header) -> getLastModifiedTime(header))(collection.breakOut)
      } else {
        Map.empty
      }
    }
  }

  def readCentralDirectory(jar: File): Unit = {
    readMetadata(jar.toPath)
  }

  def removeEntries(jarFile: File, classes: Iterable[String]): Unit = {
    val cleanPaths = classes.map(_.replace("\\", "/").stripPrefix("/"))
    removeEntries(jarFile.toPath, cleanPaths.toSet)
  }

  def mergeArchives(into: File, from: File): Unit = {
    mergeArchives(into.toPath, from.toPath)
  }

  type Metadata
  type Header

  protected def removeEntries(path: Path, toRemove: Set[String]): Unit = {
    val metadata = readMetadata(path)
    removeEntriesFromMetadata(metadata, toRemove)
    val writeOffset = truncateMetadata(metadata, path)
    finalizeZip(metadata, path, writeOffset)
  }

  protected def removeEntriesFromMetadata(metadata: Metadata, toRemove: Set[String]): Unit = {
    val headers = getHeaders(metadata)
    val sanitizedToRemove = toRemove.map(_.stripPrefix("/"))
    val clearedHeaders = headers.filterNot(header => sanitizedToRemove.contains(getFileName(header)))
    setHeaders(metadata, clearedHeaders)
  }

  protected def mergeArchives(target: Path, source: Path): Unit = {
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

  protected def mergeHeaders(
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
    new FileOutputStream(path.toFile, true)
      .getChannel
      .truncate(sizeAfterTruncate)
      .close()
    sizeAfterTruncate
  }

  protected def finalizeZip(
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

  protected def transferAll(
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
      val transferred = targetFile.transferFrom(sourceFile, /*position =*/ offset, /*count = */ remaining)
      offset += transferred
      remaining -= transferred
    }
    sourceFile.close()
    targetFile.close()
  }

  protected def openFileForReading(path: Path): ReadableByteChannel = {
    Channels.newChannel(new BufferedInputStream(Files.newInputStream(path)))
  }

  protected def openFileForWriting(path: Path): FileChannel = {
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
