package com.github.samtebbs33.baldr

import java.io.{File, FileOutputStream}
import java.util.zip.ZipOutputStream

import scala.collection.mutable
import scala.collection.JavaConversions._
import scala.collection.immutable

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

  def applyChanges(hash: String, contentList: ContentList): Unit = {
    val changes = changeList(hash)
    val ordering = new Ordering[(Int, Int)] {
      override def compare(x: (Int, Int), y: (Int, Int)): Int = x._1.compareTo(y._1)
    }
    val offsetSet = new mutable.TreeSet[(Int, Int)]()(ordering)
    def getLineOffset(line: Int) = offsetSet.takeWhile(_._1 <= line).map(_._1).sum
    // Apply changes
    changes.foreach {
      case (file, list) =>
        offsetSet.clear()
        list.foreach {
          case (addition, line, data) =>
            val offsetChange = if(addition) 1 else -1
            // Get list for file from contentList, else add it
            val content = contentList.find(_._1.equals(file)) match {
              case Some(list) => list._2
              case _ =>
                val list = new mutable.MutableList[String]()
                val pair = (file, list)
                contentList += pair
                list
            }
            // Update file contents
            val lineWithOffset = getLineOffset(line)
            if(addition) content.add(lineWithOffset, data)
            else content.remove(lineWithOffset)
            // Update offset set with offset created by this change
            offsetSet.find(_._1 == line) match {
              case Some((l, off)) => offsetSet.add((l, off + offsetChange))
              case None => offsetSet.add((line, offsetChange))
            }
        }
    }
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
