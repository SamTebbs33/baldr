package com.github.samtebbs33.baldr

import java.io.{File, FileOutputStream}
import java.util.zip.ZipOutputStream

import scala.collection.mutable
import scala.collection.JavaConversions._

/**
  * Created by samtebbs on 09/09/2016.
  */
class Save(val hash: String) {

  val metaAttributes = new mutable.HashMap[String, String]()

  def addMetaAttribute(name: String, value: String) = metaAttributes.put(name, value)

  def write(files: Array[File]): Unit = {
    if(Branch.current.savesSinceCache >= Save.cacheInterval) {
      Cache.buildCache(files, hash)
      Branch.current.savesSinceCache = 0
    } else Branch.current.savesSinceCache += 1
    val metaFile = new File(Save.savesDir, hash + Baldr.saveMetaExtension)
    metaFile.createNewFile()
    metaAttributes.foreach(pair ⇒ IO.appendToFile(metaFile, pair._1 + "=" + pair._2 + System.lineSeparator()))
    val zipFile = new File(Save.savesDir, hash + ".zip")
    zipFile.createNewFile()
    val zos = new ZipOutputStream(new FileOutputStream(zipFile))
    def addFiles(list: Array[File], path: String): Unit = list.foreach(child ⇒ {
      if (!child.isDirectory) IO.writeFileToZip(zos, path, child)
      else addFiles(child.listFiles(), path + File.separator + child.getName)
    })
    addFiles(files, "")
    zos.close()
  }

}

object Save {

  type ContentList = scala.collection.mutable.MutableList[(File, mutable.MutableList[String])]
  type DiffList = mutable.MutableList[(Boolean, Int, String)]

  def getStateAtSave(hash: String): ContentList = {
    // Go through each save from head to target, reversing changes made in each
    val save = Save.load(hash)
    // Go through parents until cache is found, adding saves to saveStack on the way
    val saveStack = new mutable.Stack[String]()
    val contentList = findCache(save)
    def findCache(save: Save): ContentList = {
      Cache.get(save.hash) match {
        case Some(cache) => cache
        case _ => saveStack.push(save.hash)
          save.parent match {
            case "" => new ContentList
            case parent => findCache(Save.load(parent))
          }
      }
    }
    // Apply changes from each save
    saveStack.foreach(hash => Save.applyChanges(hash, contentList))
    contentList
  }

  val savesDir = new File(Baldr.baldrDir.getAbsolutePath, "saves")
  val cacheInterval = 5

  def load(hash: String): Save = {
    val save = new Save(hash)
    val saveFile = new File(savesDir, hash + ".txt")
    val properties = new PropertiesFile(saveFile)
    save.addMetaAttribute("parent", properties("parent"))
    save.addMetaAttribute("author", properties("author"))
    save.addMetaAttribute("message", properties("message"))
    save
  }

  def changeList(hash: String): mutable.Map[File, mutable.MutableList[(Boolean, Int, String)]] = {
    val dir = new File(savesDir, hash)
    val map = new mutable.HashMap[File, DiffList]()
    val indexFile = new File(dir, "index.txt")
    IO.readLines(indexFile).map(_.split("=")).foreach {
      case Array(path, num) ⇒ {
        val fileChanges = new DiffList
        val file = new File(dir, num + ".txt")
        IO.readLines(file).map(_.split(":")).foreach {
          case Array(changeType, changeLine, changeData@_*) ⇒
            val tuple = (changeType.equals("+"), changeLine.toString.toInt, changeData.mkString(":"))
            fileChanges += tuple
        }
        map.put(new File(path), fileChanges)
      }
    }
    map
  }

}
