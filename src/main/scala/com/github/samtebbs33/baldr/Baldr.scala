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
      val hash = Save.randHash()
      val save = new Save(hash, msg, author, date, currentHead)
      save.write(files.toArray)
      Branch.updateHead(newHead = hash)
      staging.clear()
    }
  }

  def revert(hash: String): Unit = {
    val contentList = Save.getStateAtSave(hash)
    // Delete working directory and replace with versions from save
    root.listFiles(baldrDirFilter).foreach(IO.delete)
    // Write contentList to files
    contentList.foreach {
      case (file, lines) => if(lines.nonEmpty) IO.writeLines(file, lines.toList)
    }
  }

  def listSaves(): Unit = {
    Save.savesDir.listFiles().foreach(dir ⇒ {
      val save = new Save(dir.getName, new PropertiesFile(new File(dir, "meta.txt")))
      printf("%n* Date: %s, #%s%n", save.date, save.hash)
      Map("message" -> save.message, "author" -> save.author).foreach(pair ⇒ println(s"${pair._1}: ${pair._2}"))
    })
  }

  def stage(path: String): Unit = {
    val file = new File(path)
    if(ignore.has(path)) println("File is ignored")
    else if(!file.exists()) println("File does not exist")
    else staging.add(path)
  }

  def diff(arg1: String, arg2: String): Unit = {
    def getContentMap(str: String) = Save.parseHash(arg1) match {
      case Some(hash) => Some(Save.getStateAtSave(hash))
      case None => IO.contentMap(new File(str))
    }
    val arg1Content = getContentMap(arg1)
    val arg2Content = getContentMap(arg2)
    (arg1Content, arg2Content) match {
      case (x, y) if x.isEmpty || y.isEmpty => println("Invalid argument/s")
      case (Some(content1), Some(content2)) =>
        val intersection = content1.filter(content2.contains(_)).map(pair => (pair._1, (pair._2, content2(pair._1))))
        val diffs = intersection.map {
          case (file, (lines1, lines2)) => (file, Save.diff(lines1.toList, lines2.toList))
        }
        diffs.foreach{
          case (file, diff) =>
            println(s"%n-> ${file.toString}")
            diff.foreach(change => println(Save.changetoStr(change)))
        }
    }
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
      case "diff" => diff(args(1), args(2))
    }
    ignore.writeChanges()
    staging.writeChanges()
    Branch.writeChanges()
  }

}
