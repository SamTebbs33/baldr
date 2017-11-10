package com.github.samtebbs33.baldr

import java.io.File
import java.nio.file.Files
import java.util.Properties

import scala.collection.mutable
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

/**
  * Created by samtebbs on 09/09/2016.
  */
class Branch(val name: String, var head: String, var savesSinceCache: Int = 0, val propertiesFile: PropertiesFile) {
  println(name)

  def write(): Unit = {
    propertiesFile.set("name", name)
    propertiesFile.set("head", head)
    propertiesFile.set("sinceCache", savesSinceCache.toString)
    propertiesFile.write()
  }

}

object Branch {

  val branchesDir = new File(Baldr.baldrDir, "branches")
  val metaFile = new PropertiesFile(new File(Baldr.baldrDir, "meta.txt"))
  var current: Branch = _
  val branches = new mutable.MutableList[Branch]()

  def updateHead(branch: Branch = current, newHead: String): Unit = branch.head = newHead

  def head = current match {
    case null ⇒ ""
    case x ⇒ x.head
  }

  def getBranch(branchName: String) = branches.find(b => b.name.equals(branchName))

  def branchExists(branchName: String) = getBranch(branchName) match {
    case Some(_) => true
    case None => false
  }

  def writeChanges(): Unit = {
    branches.foreach(_.write())
    metaFile.write()
  }

  def setCurrentBranch(branch: Branch) = {
    current = branch
    metaFile.set("branch", current.name)
  }

  def createBranch(name: String, head: String = head): Branch = {
    val branch = new Branch(name, head, propertiesFile = new PropertiesFile(new File(branchesDir, name + ".txt")))
    Branch.branches += branch
    branch
  }

  def loadBranches(): Unit = {
    branchesDir.mkdir()
    val branchFiles = branchesDir.listFiles()
    // Load all branches
    branchFiles.map(new PropertiesFile(_)).foreach(p ⇒ branches += new Branch(p.get("name"), p.get("head"), p.update("sinceCache", "0").toInt, p))
    // Load current branch
    val currentBranch = metaFile.get("branch")
    current = branches.find(_.name.equals(currentBranch)).get
  }

}
