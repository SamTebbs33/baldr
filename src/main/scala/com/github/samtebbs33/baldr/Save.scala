package com.github.samtebbs33.baldr

import java.io.{File, FileOutputStream}
import java.util.zip.ZipOutputStream

import scala.collection.{immutable, mutable}
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

/**
  * Created by samtebbs on 09/09/2016.
  */
class Save(val hash: String) {

  val metaAttributes = new mutable.HashMap[String, String]()

  def parent = metaAttributes("parent")
  def author = metaAttributes("author")
  def message = metaAttributes("message")
  def addMetaAttribute(name: String, value: String) = metaAttributes.put(name, value)

  def write(files: Array[File]): Unit = {
    if(Branch.current.savesSinceCache >= Save.cacheInterval) {
      Cache.buildCache(files, hash)
      Branch.current.savesSinceCache = 0
    } else Branch.current.savesSinceCache += 1
    val saveDir = new File(Save.savesDir, hash)
    saveDir.mkdirs()
    val metaFile = new File(saveDir, "meta.txt")
    metaFile.createNewFile()
    metaAttributes.foreach(pair ⇒ IO.appendToFile(metaFile, pair._1 + "=" + pair._2 + System.lineSeparator()))
    val changeDump = new ChangeDump(saveDir, files)
    changeDump.write()
  }

}

object Save {

  type ContentMap = scala.collection.mutable.HashMap[File, mutable.MutableList[String]]
  type DiffList = mutable.MutableList[(Boolean, Int, String)]

  def diff(oldLines: List[String], newLines: List[String]): DiffList = {
    val diffList = new mutable.MutableList[(Boolean, Int, String)]()
    def aux(oldIdx: Int, newIdx: Int): Unit = {
      var addition = false
      var changeStr = ""
      var changeLine = 0
      var n = newIdx
      var o = oldIdx
      // Set the type of change that occured on the line
      def setChangeType(a: Boolean): Unit = {
        changeLine = if(a) n else o
        changeStr = if(a) newLines(changeLine) else oldLines(changeLine)
        addition = a
      }
      // If we've run over both lists then just return
      if(o >= oldLines.size && newIdx >= newLines.size) return
      // If no more lines exist in the old list but more exist in the new list, then set type as an addition
      else if(o >= oldLines.size) {
        setChangeType(true)
        n += 1
      }
      // If no more lines exist in the new list but more exist in the old list, then set type as a deletion
      else if(n >= newLines.size) {
        setChangeType(false)
        o += 1
      } else {
        val oldLine = oldLines(o)
        val newLine = newLines(n)
        // If the new line and old line are not equal
        if(!oldLine.equals(newLine)) {
          // Find out if old line exists further down in the new line list. Optimise by caching strings and the lines they appear on before hand
          newLines.zipWithIndex.takeRight(newLines.size - n - 1).exists(_._1.equals(oldLine)) match {
            // If it does then the new line was added, rather than the old line being deleted
            case true =>
              setChangeType(true)
              n += 1
            // If it doesn't then the old line was deleted, rather than the new line being added
            case false =>
              setChangeType(false)
              o += 1
          }
        } else {
          // If both are equal then ignore this line and skip ahead
          aux(o + 1, n + 1)
          return
        }
      }
      // Add the change to the diff list
      val change = (addition, changeLine, changeStr)
      diffList += change
      // Recurse over the rest of the line lists
      aux(o, n)
    }
    // Start from beginning of both lists
    aux(0, 0)
    diffList
  }

  def getStateAtSave(hash: String): ContentMap = {
    if(hash.isEmpty) return new ContentMap
    val save = Save.load(hash)
    // Go through parents until cache is found, adding saves to saveStack on the way
    val saveStack = new mutable.Stack[String]()
    def findCache(save: Save): ContentMap = {
      Cache.get(save.hash) match {
        case Some(cache) => cache
        case _ => saveStack.push(save.hash)
          save.parent match {
            case "" => new ContentMap
            case parent => findCache(Save.load(parent))
          }
      }
    }
    val contentList = findCache(save)
    // Apply changes from each save
    saveStack.foreach(hash => {
      Save.applyChanges(hash, contentList)
    })
    contentList
  }

  def applyChanges(hash: String, contentList: ContentMap): Unit = {
    new ChangeDump(new File(savesDir, hash)).applyChanges(contentList)
  }

  val savesDir = new File(Baldr.baldrDir.getAbsolutePath, "saves")
  val cacheInterval = 5

  def load(hash: String): Save = {
    val save = new Save(hash)
    val saveDir = new File(savesDir, hash)
    val saveFile = new File(savesDir, "meta.txt")
    val properties = new PropertiesFile(saveFile)
    save.addMetaAttribute("parent", properties.get("parent"))
    save.addMetaAttribute("author", properties.get("author"))
    save.addMetaAttribute("message", properties.get("message"))
    save
  }

  def changeList(hash: String): mutable.Map[File, mutable.MutableList[(Boolean, Int, String)]] = {
    val dir = new File(savesDir, hash)
    val changeDump = new ChangeDump(dir)
    changeDump.changeList()
  }

}
