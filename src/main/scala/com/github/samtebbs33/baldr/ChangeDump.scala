package com.github.samtebbs33.baldr

import java.io.File

import com.github.samtebbs33.baldr.Save.{ContentMap, DiffList}

import scala.collection.mutable
import scala.collection.JavaConversions._
import scala.collection.mutable.MutableList

/**
  * Created by samtebbs on 14/09/2016.
  */
class ChangeDump(dir: File) {

  def write(files: Array[File]): Unit = {
    // Get state at last save
    val state = Save.getStateAtSave(Branch.head)
    var fileCounter = 0
    // Index file stores bindings between file paths and IDs
    val indexFile = new File(dir, "index.txt")
    indexFile.createNewFile()
    // Add diff list for each file
    def addFiles(list: Array[File], path: String): Unit = list.foreach(child ⇒ {
      if (!child.isDirectory) {
        // Get lines from file in its last state
        val oldLines = state.getOrElseUpdate(child, new mutable.MutableList[String]()).toList
        // Get lines from file in working directory
        val newLines = IO.readLines(child).toList
        // Get changes between version in last save and version in working dir
        val changes = Save.diff(oldLines, newLines)
        // If there were changes
        if(changes.nonEmpty) {
          // Add file to index
          IO.appendToFile(indexFile, path + child.getName + "=" + fileCounter + "\n")
          val changeFile = new File(dir, fileCounter + ".txt")
          changeFile.createNewFile()
          // Print changes to file
          changes.foreach {
            case (addition, line, data) =>
              val changeStr = (if(addition) "+" else "-") + ":" + line + ":" + data
              IO.appendToFile(changeFile, changeStr + "\n")
          }
          fileCounter += 1
        }
      } else addFiles(child.listFiles(), path + File.separator + child.getName)
    })
    addFiles(files, "")
  }

  def changeList(): mutable.HashMap[File, DiffList] = {
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

  def applyChanges(contentList: ContentMap): Unit = {
    val changes = changeList()
    val ordering = new Ordering[(Int, Int)] {
      override def compare(x: (Int, Int), y: (Int, Int)): Int = x._1.compareTo(y._1)
    }
    val offsetSet = new mutable.TreeSet[(Int, Int)]()(ordering)
    def getLineOffset(line: Int) = offsetSet.takeWhile(_._1 < line).map(_._1).sum + line
    // Apply changes
    changes.foreach {
      case (file, list) =>
        offsetSet.clear()
        list.foreach {
          case (addition, line, data) =>
            val offsetChange = if(addition) 1 else -1
            // Get list for file from contentList, else add it
            var content = contentList.getOrElseUpdate(file, new mutable.MutableList[String]())
            def insert(idx: Int, line: String): Unit = {
              val pair = content.splitAt(idx) // split it at the appropriate index into two lists.
              content = pair._1 ++ mutable.MutableList(line) ++ pair._2
            }
            // Update file contents
            val lineWithOffset = getLineOffset(line)
            if(addition) insert(lineWithOffset, data)
            else content.remove(lineWithOffset)
            contentList.put(file, content)
            // Update offset set with offset created by this change
            offsetSet.find(_._1 == line) match {
              case Some((l, off)) => offsetSet.add((l, off + offsetChange))
              case None => offsetSet.add((line, offsetChange))
            }
        }
    }
  }

}
