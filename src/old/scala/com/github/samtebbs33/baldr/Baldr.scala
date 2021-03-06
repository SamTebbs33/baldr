package com.github.samtebbs33.baldr

import java.io._
import java.util.Date

import scala.io.StdIn

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
  val tracking = new FileList(new File(baldrDir, "tracking.txt"))
  val ignore = new FileList(new File(".baldr_ignore"))

  val baldrDirFilter = new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = !name.equals(dirName)
  }

  def init(): Unit = {
    baldrDir.exists() match {
      case true ⇒ println(".baldr dir already exists")
      case _ ⇒ {
        baldrDir.mkdir()
        Branch.setCurrentBranch(Branch.createBranch(masterBranchName))
        println("Initialised repository in " + baldrDir.getAbsolutePath)
      }
    }
  }

  def resetFile(filePath: String, hash: String): Unit = {
      val file = new File(filePath)
      // TODO Really terrible and inefficient. Maybe only load relevant file state, not the state of the whole save, in future?
      val content = Save.getStateAtSave(hash)
      content.get(file) match {
        case Some(lines) => {
          IO.delete(file)
          IO.writeLines(file, lines.toList)
        }
        case None => println(s"The file '$filePath' was not tracked at this save")
      }
  }

  def track(filePath: String): Unit = {
    tracking.add(filePath)
  }

  def untrack(filePath: String): Unit = {
    tracking.remove(filePath);
  }

  def unstage(filePath: String): Unit = {
    staging.remove(filePath)
  }

  def splitArgs(input: String): Array[String] = {
    def aux(input: Seq[Char], inQuotes: Boolean, progress: String, acc: List[String]): List[String] = input match {
      case Seq(prefix, suffix@_*) => prefix match {
        case '\"' if inQuotes => aux(suffix, false, "", progress :: acc)
        case '\"' if progress.isEmpty => aux(suffix, true, "", acc)
        case '\"' => aux(suffix, true, "", progress :: acc)
        case ' ' if !inQuotes && progress.isEmpty => aux(suffix, false, "", acc)
        case ' ' if !inQuotes => aux(suffix, false, "", progress :: acc)
        case ch => aux(suffix, inQuotes, progress + ch, acc)
      }
      case _ if progress.isEmpty => acc
      case _ => progress :: acc
    }
    aux(input.toSeq, false, "", List[String]()).reverse.toArray
  }

  def repl(args: Array[String]): Unit = {
    def prompt(): String = {
      print("> ")
      StdIn.readLine()
    }

    while(prompt() match {
      case null => false
      case input =>
        val split = splitArgs(input)
        Command.accept(split(0), split.slice(1, split.length))
        writeChanges()
        true
    }){}
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
    tracking.list.foreach(IO.delete)
    // Write contentList to tracked files
    contentList.filter(p => tracking.list.contains(p._1)).foreach {
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
    else {
      track(path)
      staging.add(path)
    }
  }

  def writeChanges(): Unit = {
    ignore.writeChanges()
    staging.writeChanges()
    tracking.writeChanges()
    Branch.writeChanges()
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
    tracking.createFileAndLoad()
    Branch.loadBranches()
    Command.accept(args(0), args.slice(1, args.length))
    writeChanges()
  }

}
