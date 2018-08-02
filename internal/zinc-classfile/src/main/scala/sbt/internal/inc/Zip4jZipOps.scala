package sbt.internal.inc

import java.io.RandomAccessFile

import net.lingala.zip4j.core.{ HeaderReader, HeaderWriter }
import java.nio.channels.{ FileChannel, Channels }
import java.nio.file.{ Path, Files }
import java.util.ArrayList

import scala.collection.JavaConverters._
import net.lingala.zip4j.model.{ ZipModel, FileHeader }

object Zip4jZipOps {

  type CentralDirectory = ZipModel

  def removeEntries(path: Path, toRemove: Set[String]): Unit = {
    val centralDir = readCentralDir(path)
    removeEntriesFromCentralDir(centralDir, toRemove)
    val file = openFile(path)
    truncateCentralDir(centralDir, file)
    val writeOffset = startOfCentralDir(centralDir)
    finalizeZip(centralDir, file, writeOffset)
  }

  private def removeEntriesFromCentralDir(centralDir: CentralDirectory,
                                          toRemove: Set[String]): Unit = {
    val headers = getHeaders(centralDir)
    val sanitizedToRemove = toRemove.map(_.stripPrefix("/"))
    val clearedHeaders = headers.filterNot(header => sanitizedToRemove.contains(header.getFileName))
    centralDir.getCentralDirectory.setFileHeaders(asArrayList(clearedHeaders))
  }

  def mergeArchives(target: Path, source: Path): Unit = {
    val targetCentralDir = readCentralDir(target)
    val sourceCentralDir = readCentralDir(source)

    val targetFile = openFile(target)
    val sourceFile = openFile(source)

    truncateCentralDir(targetCentralDir, targetFile)

    // "source" starts where "target" ends
    val sourceStart = targetFile.size()
    // "source" is as long as from its beginning till the start of central dir
    val sourceLength = startOfCentralDir(sourceCentralDir)

    transferAll(from = sourceFile,
                to = targetFile,
                startPos = sourceStart,
                bytesToTransfer = sourceLength)
    sourceFile.close()

    val mergedHeaders = mergeHeaders(targetCentralDir, sourceCentralDir, sourceStart)
    targetCentralDir.getCentralDirectory.setFileHeaders(asArrayList(mergedHeaders))

    val centralDirStart = sourceStart + sourceLength
    targetCentralDir.getEndCentralDirRecord.setOffsetOfStartOfCentralDir(centralDirStart)

    finalizeZip(targetCentralDir, targetFile, centralDirStart)

    Files.delete(source)
  }

  private def mergeHeaders(
      targetModel: CentralDirectory,
      sourceModel: CentralDirectory,
      sourceStart: Long
  ): Seq[FileHeader] = {
    val sourceHeaders = getHeaders(sourceModel)
    sourceHeaders.foreach { header =>
      // potentially offsets should be updated for each header
      // not only in central directory but a valid zip tool
      // should not rely on that unless the file is corrupted
      val currentOffset = header.getOffsetLocalHeader
      val newOffset = currentOffset + sourceStart
      header.setOffsetLocalHeader(newOffset)
    }

    // override files from target with files from source
    val sourceNames = sourceHeaders.map(_.getFileName).toSet
    val targetHeaders = getHeaders(targetModel).filterNot(h => sourceNames.contains(h.getFileName))

    targetHeaders ++ sourceHeaders
  }

  private def readCentralDir(path: Path): CentralDirectory = {
    val file = new RandomAccessFile(path.toFile, "rw")
    val headerReader = new HeaderReader(file)
    val centralDir = headerReader.readAllHeaders()
    file.close()
    centralDir
  }

  private def startOfCentralDir(centralDir: CentralDirectory) = {
    centralDir.getEndCentralDirRecord.getOffsetOfStartOfCentralDir
  }

  private def getHeaders(centralDir: CentralDirectory): Seq[FileHeader] = {
    centralDir.getCentralDirectory.getFileHeaders.asInstanceOf[ArrayList[FileHeader]].asScala
  }

  private def truncateCentralDir(centralDir: CentralDirectory,
                                 channel: FileChannel): FileChannel = {
    channel.truncate(startOfCentralDir(centralDir))
  }

  private def finalizeZip(centralDir: CentralDirectory,
                          channel: FileChannel,
                          centralDirStart: Long): Unit = {
    val outputStream = Channels.newOutputStream(channel.position(centralDirStart))
    val headerWriter = new HeaderWriter
    headerWriter.finalizeZipFile(centralDir, outputStream)
    channel.close()
  }

  private def openFile(path: Path) = {
    new RandomAccessFile(path.toFile, "rw").getChannel
  }

  private def transferAll(from: FileChannel,
                          to: FileChannel,
                          startPos: Long,
                          bytesToTransfer: Long): Unit = {
    var remaining = bytesToTransfer
    var offset = startPos
    while (remaining > 0) {
      val transferred = to.transferFrom(from, /*position =*/ offset, /*count = */ remaining)
      offset += transferred
      remaining -= transferred
    }
  }

  private def asArrayList[A](clearedHeaders: Seq[A]): ArrayList[A] = {
    new ArrayList[A](clearedHeaders.asJava)
  }

}
