package com.github.samtebbs33.baldr

import java.io.File

import scala.collection.mutable

/**
  * Created by samtebbs on 14/09/2016.
  */
class Stash(val id: Int = Stash.nextStashID) {

  def write(files: Array[File]): Unit = {
    val dir = new File(Stash.stashDir, id.toString)
    dir.mkdir()
    val changeDump = new ChangeDump(dir)
    changeDump.write(files)
  }

}

object Stash {

  val stack = new mutable.Stack[Stash]()
  val stashDir = new File(Baldr.baldrDir, "stashes")

  def nextStashID = if(stack.isEmpty) 0 else stack.top.id + 1

  def pop(): Unit = {
    if(stack.isEmpty) println("No stashes have been made")
    else {

    }
  }

  def loadStashes(): Unit = {
    stashDir.mkdir()
    stashDir.listFiles().filter(_.isDirectory).sortBy(_.getName).reverse.map(_.getName).foreach(id ⇒ stack.push(new Stash(id.toInt)))
  }
}
