package com.github.samtebbs33.baldr

import java.io._
import java.nio.file.{Files, StandardOpenOption}
import java.util.Date
import java.util.zip.{ZipEntry, ZipInputStream, ZipOutputStream}

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
  val author = System.getProperty("user.name")
  val staging = new FileList(new File(baldrDir, "staging.txt"))
  val ignore = new FileList(new File(".baldr_ignore"))

  val baldrDirFilter = new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = !name.equals(dirName)
  }

  def init(): Unit = {
    baldrDir.exists() match {
      case true ⇒ println(".baldr dir already exists")
      case _ ⇒ {
        baldrDir.mkdir()
        Branch.current = Branch.createBranch(masterBranchName)
        println("Initialised repository in " + baldrDir.getAbsolutePath)
      }
    }
  }

  def save(msg: String): Unit = {
    val files = staging.list
    val currentHead = Branch.head
    if(files.isEmpty) println("No files staged")
    else {
      val date = new Date()
      Save.savesDir.mkdirs()
      val hash = date.getTime
      val save = new Save(hash.toString)
      save.addMetaAttribute("message", msg)
      save.addMetaAttribute("author", author)
      save.addMetaAttribute("parent", currentHead)
      save.write(files.toArray)
      staging.clear()
    }
  }

  def revert(hash: String): Unit = {
    val saveFile = new File(Save.savesDir.getAbsolutePath, hash + ".zip")
    if(saveFile.exists()) {
      // Extract save
      val zis = new ZipInputStream(new FileInputStream(saveFile))
      var entry = zis.getNextEntry
      while (entry != null) {
        // Delete working directory copy
        new File(entry.getName).delete()
        IO.writeZipEntryToFile(entry, zis, root)
        entry = zis.getNextEntry
      }
      zis.close()
    } else println("Save doesn't exist")
  }

  def listSaves(): Unit = {
    Save.savesDir.listFiles(new FilenameFilter {
      override def accept(dir: File, name: String): Boolean = name.endsWith(saveMetaExtension)
    }).foreach(file ⇒ {
      val hash = file.getName.replaceAll("(?:\\.)(?:[0-9]|[a-z]|[A-Z])+", "")
      val date = new Date(hash.toLong)
      printf("%n* Date: %s, #%s%n", date.toString, hash)
      Files.readAllLines(file.toPath).map(_.split("=")).filter(_.length > 1).foreach(line ⇒ println(line(0) + " = " + line(1)))
    })
  }

  def stage(path: String): Unit = {
    val file = new File(path)
    if(ignore.has(path)) println("File is ignored")
    else if(!file.exists()) println("File does not exist")
    else staging.add(path)
  }

  def main(args: Array[String]): Unit = {
    if(args.length == 0) return
    val cmd = args(0)
    if(!cmd.equals("init") && !baldrDir.exists()) {
      println("Not initialised, run 'baldr init' to initialise a baldr repository here")
      return
    } else if(cmd.equals("init")) init()
    ignore.createFileAndLoad()
    staging.createFileAndLoad()
    Branch.loadBranches()
    cmd match {
      case "init" ⇒
      case "save" ⇒ save(args(1))
      case "revert" ⇒ revert(args(1))
      case "list" ⇒ listSaves()
      case "stage" => stage(args(1))
      case "unstage" => staging.remove(args(1))
      case "ignore" => ignore.add(args(1))
      case "ack" => ignore.remove(args(1))
    }
    ignore.writeChanges()
    staging.writeChanges()
    Branch.writeChanges()
  }

}
