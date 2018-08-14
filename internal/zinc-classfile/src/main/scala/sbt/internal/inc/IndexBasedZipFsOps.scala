package sbt.internal.inc

import java.io.OutputStream
import java.nio.file.Path

import sbt.internal.inc.zip.ZipCentralDir

import scala.collection.JavaConverters._

object IndexBasedZipFsOps extends IndexBasedZipOps {
  override type CentralDir = ZipCentralDir
  override type Header = ZipCentralDir.Entry

  protected def readCentralDir(path: Path): CentralDir = {
    new ZipCentralDir(path)
  }

  protected def getCentralDirStart(centralDir: CentralDir): Long = {
    centralDir.getCentralDirStart
  }

  protected def setCentralDirStart(centralDir: CentralDir, centralDirStart: Long): Unit = {
    centralDir.setCentralDirStart(centralDirStart)
  }

  protected def getHeaders(centralDir: CentralDir): Seq[Header] = {
    centralDir.getHeaders.asScala
  }
  protected def setHeaders(centralDir: CentralDir, headers: Seq[Header]): Unit = {
    centralDir.setHeaders(new java.util.ArrayList[Header](headers.asJava))
  }

  protected def getFileName(header: Header): String = {
    header.getName
  }

  protected def getFileOffset(header: Header): Long = {
    header.getEntryOffset
  }

  protected def setFileOffset(header: Header, offset: Long): Unit = {
    header.setEntryOffset(offset)
  }

  protected def getLastModifiedTime(header: Header): Long = {
    header.getLastModifiedTime
  }

  protected def writeCentralDir(centralDir: CentralDir, outputStream: OutputStream): Unit = {
    centralDir.dump(outputStream)
  }
}
