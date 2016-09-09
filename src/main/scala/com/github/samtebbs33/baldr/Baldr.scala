package com.github.samtebbs33.baldr

import java.io._
import java.nio.file.{Files, StandardOpenOption}
import java.util.Date
import java.util.zip.{ZipEntry, ZipInputStream, ZipOutputStream}

import com.github.samtebbs33.baldr.Checksum

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.collection.mutable

/**
  * Created by samtebbs on 08/09/2016.
  */
object Baldr {

  val saveMetaExtension = ".txt"
  val dirName = ".baldr"
  val masterBranchName = "master"
  var baldrDir = new File(dirName)
  val root = new File(".")
  val branchesFile = new File(baldrDir.getName, "branches.txt")
  val savesDir = new File(baldrDir.getAbsolutePath, "saves")
  val ignoreFile = new File(".baldr_ignore")
  val stagingFile = new File(baldrDir, "staging.txt")
  val ignoredFiles = if (ignoreFile.exists()) Files.readAllLines(ignoreFile.toPath).toArray else new Array[String](0)
  val currentBranch = masterBranchName // TODO: Read from file
  val baldrDirFilter = new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = !name.equals(dirName)
  }

  def createBranch(name: String): Unit = {
    branchesFile.createNewFile()
    appendToFile(branchesFile, name + "=" + head)
    println("Created branch " + name)
  }

  def init(): Unit = {
    baldrDir.exists() match {
      case true ⇒ println(".baldr dir already exists")
      case _ ⇒ {
        baldrDir.mkdir()
        createBranch(masterBranchName)
        println("Initialised repository in " + baldrDir.getAbsolutePath)
      }
    }
  }

  def appendToFile(file: File, s: String) = Files.write(file.toPath, s.getBytes, StandardOpenOption.APPEND)

  def stagedFiles: Array[File] = Files.readAllLines(stagingFile.toPath).map(new File(_)).toArray

  def clearStagingFile() = new FileOutputStream(stagingFile).close()

  def fileIsIgnored(path: String) = ignoredFiles.exists(_.equals(path))

  def head = Files.readAllLines(branchesFile.toPath).find(_.startsWith(currentBranch + "=")) match {
    case Some(line) => line.split("=") match {
      case Array(_, hash) => hash
      case _ => ""
    }
    case _ => ""
  }

  def updateBranchHead(branch: String, newHead: String, prevHead: String): Unit = {
    removeLineFromFile(branchesFile, branch + "=" + prevHead)
    appendToFile(branchesFile, "\n" + branch + "=" + newHead)
  }

  def save(msg: String): Unit = {
    stagingFile.createNewFile()
    val files = stagedFiles
    val currentHead = head
    if(files.isEmpty) println("No files staged")
    else {
      val date = new Date()
      savesDir.mkdirs()
      val hash = date.getTime
      val metaFile = new File(savesDir, hash + saveMetaExtension)
      metaFile.createNewFile()
      appendToFile(metaFile, "msg=" + msg)
      appendToFile(metaFile, "\nparent=" + currentHead)
      updateBranchHead(currentBranch, hash.toString, currentHead)
      val zipFile = new File(savesDir, hash + ".zip")
      zipFile.createNewFile()
      val zos = new ZipOutputStream(new FileOutputStream(zipFile))
      def addFiles(list: Array[File], path: String): Unit = list.foreach(child ⇒ {
        if (!child.isDirectory) writeFileToZip(zos, path, child)
        else addFiles(child.listFiles(), path + File.separator + child.getName)
      })
      addFiles(files, "")
      zos.close()
      clearStagingFile()
    }
  }

  def delete(file: File): Unit = {
    if (file.isDirectory) file.listFiles().foreach(delete)
    file.delete()
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

  def revert(hash: String): Unit = {
    val saveFile = new File(savesDir.getAbsolutePath, hash + ".zip")
    if(saveFile.exists()) {
      // Extract save
      val zis = new ZipInputStream(new FileInputStream(saveFile))
      var entry = zis.getNextEntry
      while (entry != null) {
        // Delete working directory copy
        new File(entry.getName).delete()
        writeZipEntryToFile(entry, zis, root)
        entry = zis.getNextEntry
      }
      zis.close()
    } else println("Save doesn't exist")
  }

  def listSaves(): Unit = {
    savesDir.listFiles(new FilenameFilter {
      override def accept(dir: File, name: String): Boolean = name.endsWith(saveMetaExtension)
    }).foreach(file ⇒ {
      val hash = file.getName.replaceAll("(?:\\.)(?:[0-9]|[a-z]|[A-Z])+", "")
      val date = new Date(hash.toLong)
      printf("%n* Date: %s, #%s%n", date.toString, hash)
      Files.readAllLines(file.toPath).map(_.split("=")).filter(_.length > 1).foreach(line ⇒ println(line(0) + " = " + line(1)))
    })
  }

  def stage(path: String): Unit = {
    stagingFile.createNewFile()
    val file = new File(path)
    if(fileIsIgnored(path)) println("File is ignored by '" + ignoreFile.getName + "'")
    else if(!file.exists()) println("File does not exist")
    else appendToFile(stagingFile, path)
  }

  def removeLineFromFile(file: File, line: String): Unit = {
    val lines = Files.readAllLines(file.toPath)
    val writer = new PrintWriter(file)
    lines.filter(!_.equals(line)).foreach(writer.println)
    writer.close()
  }

  def main(args: Array[String]): Unit = {
    if(args.length == 0) return
    if(!args(0).equals("init") && !baldrDir.exists()) {
      println("Not initialised, run 'baldr init' to initialise a baldr repository here")
      return
    }
    args(0) match {
      case "init" ⇒ init()
      case "save" ⇒ save(args(1))
      case "revert" ⇒ revert(args(1))
      case "list" ⇒ listSaves()
      case "stage" => stage(args(1))
      case "ignore" => {
        ignoreFile.createNewFile()
        appendToFile(ignoreFile, args(1))
      }
      case "ack" => {
        if(!ignoreFile.exists()) println("'" + ignoreFile + "' doesn't exist")
        else removeLineFromFile(ignoreFile, args(1))
      }
    }
  }

}
