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

  def write(): Unit ={
    propertiesFile("name", name)
    propertiesFile("head", head)
    propertiesFile("sinceCache", savesSinceCache.toString)
    propertiesFile.write()
  }

}

object Branch {

  val branchesDir = new File(Baldr.baldrDir, "branches")
  val metaFile = PropertiesFile(new File(Baldr.baldrDir, "meta.txt"))
  var current: Branch = _
  val branches = new mutable.MutableList[Branch]()

  def updateHead(branch: Branch = current, newHead: String): Unit = branch.head = newHead

  def head = current match {
    case null ⇒ ""
    case x ⇒ x.head
  }

  def writeChanges(): Unit = {
    branches.foreach(_.write())
    metaFile.write()
  }

  def setCurrentBranch(branch: Branch) = {
    current = branch
    metaFile("branch", current.name)
  }

  def createBranch(name: String, head: String = head): Branch = {
    val branch = new Branch(name, head, propertiesFile = PropertiesFile(new File(branchesDir, name + ".txt")))
    Branch.branches += branch
    setCurrentBranch(branch)
    branch
  }

  def loadBranches(): Unit = {
    branchesDir.mkdir()
    val branchFiles = branchesDir.listFiles()
    // Load all branches
    branchFiles.map(PropertiesFile).foreach(p ⇒ branches += new Branch(p("name"), p("head"), p("sinceCache", "0").toInt, p))
    // Load current branch
    val currentBranch = metaFile("branch")
    current = branches.find(_.name.equals(currentBranch)).get
  }

}
