package com.github.samtebbs33.baldr

import java.io.File
import java.nio.file.Files

import scala.collection.mutable
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

/**
  * Created by samtebbs on 09/09/2016.
  */
class Branch(val name: String, var head: String)

object Branch {

  val branchesFile = new File(Baldr.baldrDir.getName, "branches.txt")
  var current: Branch = _
  val branches = new mutable.MutableList[Branch]()

  def updateHead(branch: String, newHead: String): Unit = branches.find(_.name.equals(branch)) match {
    case Some(b) ⇒ b.head = newHead
    case _ ⇒
  }

  def head = current match {
    case null ⇒ ""
    case x ⇒ x.head
  }

  def writeChanges(): Unit = {
    IO.writeLines(branchesFile, List(current.name), branches.toList.map(b ⇒ b.name + "=" + b.head))
  }

  def createBranch(name: String, head: String = head): Branch = {
    val branch = new Branch(name, head)
    Branch.branches += branch
    branch
  }

  def loadBranches(): Unit = {
    branchesFile.createNewFile()
    val lines = Files.readAllLines(branchesFile.toPath)
    // Load all branches
    lines.filter(_.contains("=")).map(_.split("=")).foreach{
      case Array(name, hash) ⇒ branches += new Branch(name, hash)
      case Array(name) ⇒ branches += new Branch(name, "")
      case _ ⇒
    }
    // Load current branch
    lines.find(!_.contains("=")) match {
      case Some(line) ⇒ current = branches.find(_.name.equals(line)).get
      case _ ⇒
    }
  }

}
