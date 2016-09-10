package com.github.samtebbs33.baldr

import java.io.File

import scala.collection.mutable
import scala.collection.JavaConversions._

/**
  * Created by samtebbs on 10/09/2016.
  */
class PropertiesFile(file: File) {

  val properties = new mutable.HashMap[String, String]()
  if(file.exists()) IO.readLines(file).map(_.split("=")).filter(_.length > 0).map{
    case Array(name, value) ⇒ (name, value)
    case Array(name) ⇒ (name, "")
  }.foreach(pair ⇒ properties.put(pair._1, pair._2))

  def write(): Unit = {
    file.createNewFile()
    IO.writeLines(file, properties.map(pair ⇒ pair._1 + "=" + pair._2).toList)
  }

  def apply(key: String): String = properties(key)
  def apply(key: String, default: String): String = properties.getOrElseUpdate(key, default)

}
