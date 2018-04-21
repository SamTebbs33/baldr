package com.github.samtebbs33.baldr

import java.io.{File, FileInputStream, FileOutputStream, PrintWriter}
import java.nio.file.{Files, StandardOpenOption}
import java.util.zip.{ZipEntry, ZipInputStream, ZipOutputStream}

import scala.collection.JavaConversions._

/**
  * Created by samtebbs on 09/09/2016.
  */
object IO {

  def writeLines(file: File, lines: List[String]*): Unit = {
    file.createNewFile()
    val writer = new PrintWriter(file)
    lines.foreach(_.foreach(writer.println))
    writer.close()
  }

  def readLines(file: File) = Files.readAllLines(file.toPath)

  def removeLineFromFile(file: File, line: String): Unit = {
    val lines = readLines(file)
    val writer = new PrintWriter(file)
    lines.filter(!_.equals(line)).foreach(writer.println)
    writer.close()
  }

  def delete(file: File): Unit = {
    if (file.isDirectory) file.listFiles().foreach(delete)
    file.delete()
  }

  def appendToFile(file: File, s: String) = Files.write(file.toPath, s.getBytes, StandardOpenOption.APPEND)

  def updateFileLine(file: File, line: String, newLine: String): Unit = {
    val lines = readLines(file)
    val writer = new PrintWriter(file)
    lines.foreach(l ⇒ writer.println(if(l.equals(line)) newLine else l))
    writer.close()
  }

  def writeFileToZip(zos: ZipOutputStream, parent: String, file: File): Unit = {
    zos.putNextEntry(new ZipEntry(parent + File.separator + file.getName))
    val buffer = new Array[Byte](1024)
    val fis = new FileInputStream(file)
    var length = fis.read(buffer)
    while (length > 0) {
      zos.write(buffer, 0, length)
      length = fis.read(buffer)
    }
    fis.close()
    zos.closeEntry()
  }

  def writeZipEntryToFile(entry: ZipEntry, zis: ZipInputStream, root: File) = {
    val file = new File(root, entry.getName)
    file.getParentFile.mkdirs()
    file.createNewFile()
    val fos = new FileOutputStream(file)
    val buffer = new Array[Byte](1024)
    var length = zis.read(buffer)
    while (length > 0) {
      fos.write(buffer, 0, length)
      length = zis.read(buffer)
    }
    fos.close()
  }

}
