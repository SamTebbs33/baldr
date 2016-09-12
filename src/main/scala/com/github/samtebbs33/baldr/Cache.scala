package com.github.samtebbs33.baldr

import java.io.{File, FileOutputStream}
import java.nio.file.Files

import com.github.samtebbs33.baldr.Save.ContentMap

import scala.collection.JavaConversions._
import scala.collection.mutable

/**
  * Created by samtebbs on 10/09/2016.
  */
object Cache {

  val cacheDir = new File(Baldr.baldrDir, "cache")
  val cacheObjectDir = new File(cacheDir, "objects")
  val cacheSaveDir = new File(cacheDir ,"saves")

  def get(hash: String): Option[ContentMap] = {
    val cacheFile = new File(cacheSaveDir, hash + ".txt")
    if(cacheFile.exists()) {
      val map = new ContentMap
      IO.readLines(cacheFile).filter(_.nonEmpty).map(line => line.splitAt(line.lastIndexOf(File.separatorChar)))
        .map(pair => (pair._1, pair._2.substring(1)))
        .foreach(pair => {
          val filePath = pair._1
          val checksum = pair._2
          val contentFile = new File(new File(cacheObjectDir, filePath), checksum)
          map.put(new File(filePath), scala.collection.mutable.MutableList(IO.readLines(contentFile):_*))
        })
      Some(map)
    } else None
  }

  def buildCache(files: Array[File], hash: String): Unit = {
    cacheObjectDir.mkdirs()
    cacheSaveDir.mkdirs()
    val cacheFile = new File(cacheSaveDir, hash + ".txt")
    cacheFile.createNewFile()
    def addFileToCache(file: File): Unit = {
      // Make the directory in which all caches for this file are put
      val fileCacheObjectDir = new File(cacheObjectDir, file.toString)
      fileCacheObjectDir.mkdirs()
      // Create the file that will stored the cached contents
      val checksum = Checksum.getChecksum(file)
      val fileCacheObjectFile = new File(fileCacheObjectDir, checksum)
      if(!fileCacheObjectFile.exists()) {
        fileCacheObjectFile.createNewFile()
        Files.copy(file.toPath, new FileOutputStream(fileCacheObjectFile))
      }
      // Add this file cache the main cache file
      IO.appendToFile(cacheFile, file.toString + File.separator + checksum + "\n")
    }
    files.foreach(addFileToCache)
  }

}
