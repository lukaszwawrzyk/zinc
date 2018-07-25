package sbt.internal.inc

import java.io.RandomAccessFile

import net.lingala.zip4j.core.{ HeaderReader, HeaderWriter }
import java.nio.channels.{ FileChannel, Channels }
import java.nio.file.{ Path, Files }
import java.util.ArrayList

import scala.collection.JavaConverters._
import net.lingala.zip4j.model.{ ZipModel, FileHeader }

object Zip4jZipOps {

  def getCentralDir(path: Path): Seq[FileHeader] = {
    getHeaders(readModel(path))
  }

  def removeEntries(path: Path, toRemove: Set[String]): Unit = {
    val model = readModel(path)
    removeEntriesFromCentralDir(model, toRemove)
    val file = openFile(path)
    truncateCentralDir(model, file)
    val writeOffset = startOfCentralDir(model)
    finalizeZip(model, file, writeOffset)
  }

  private def removeEntriesFromCentralDir(model: ZipModel, toRemove: Set[String]): Unit = {
    val headers = getHeaders(model)
    val clearedHeaders = headers.filterNot(header => toRemove.contains(header.getFileName))
    model.getCentralDirectory.setFileHeaders(asArrayList(clearedHeaders))
  }

  def mergeArchives(target: Path, source: Path): Unit = {
    val targetModel = readModel(target)
    val sourceModel = readModel(source)

    val targetFile = openFile(target)
    val sourceFile = openFile(source)

    truncateCentralDir(targetModel, targetFile)

    // "source" starts where "target" ends
    val sourceStart = targetFile.size()
    // "source" is as long as from its beginning till the start of central dir
    val sourceLength = startOfCentralDir(sourceModel)

    transferAll(from = sourceFile,
                to = targetFile,
                startPos = sourceStart,
                bytesToTransfer = sourceLength)
    sourceFile.close()

    val mergedHeaders = mergeHeaders(targetModel, sourceModel, sourceStart)
    targetModel.getCentralDirectory.setFileHeaders(asArrayList(mergedHeaders))

    val centralDirStart = sourceStart + sourceLength
    targetModel.getEndCentralDirRecord.setOffsetOfStartOfCentralDir(centralDirStart)

    finalizeZip(targetModel, targetFile, centralDirStart)

    Files.delete(source)
  }

  private def mergeHeaders(
      targetModel: ZipModel,
      sourceModel: ZipModel,
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

  private def readModel(path: Path): ZipModel = {
    val file = new RandomAccessFile(path.toFile, "rw")
    val headerReader = new HeaderReader(file)
    val model = headerReader.readAllHeaders()
    file.close()
    model
  }

  private def startOfCentralDir(model: ZipModel) = {
    model.getEndCentralDirRecord.getOffsetOfStartOfCentralDir
  }

  private def getHeaders(model: ZipModel): Seq[FileHeader] = {
    model.getCentralDirectory.getFileHeaders.asInstanceOf[ArrayList[FileHeader]].asScala
  }

  private def truncateCentralDir(model: ZipModel, channel: FileChannel): FileChannel = {
    channel.truncate(startOfCentralDir(model))
  }

  private def finalizeZip(centralDir: ZipModel,
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
