package com.github.samtebbs33.baldr

import java.io.{File, FileOutputStream, PrintWriter}

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.collection.mutable

/**
  * Created by samtebbs on 09/09/2016.
  */
class FileList(file: File) {

  var list: mutable.MutableList[File] = new mutable.MutableList[File]()

  def createFileAndLoad(): Unit = {
    file.createNewFile()
    IO.readLines(file).foreach(list += new File(_))
  }
  def writeChanges() = IO.writeLines(file, list.map(_.toString).toList)
  def add(path: String): Unit = list += new File(path)
  def remove(path: String) = list = list.filter(!_.toString.equals(path))
  def has(path: String) = list.exists(_.toString.equals(path))
  def clear() = list.clear()

}