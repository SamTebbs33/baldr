package com.github.samtebbs33.baldr

import java.io.{File, FileOutputStream}
import java.nio.file.Files

/**
  * Created by samtebbs on 10/09/2016.
  */
object Cache {

  val cacheDir = new File(Baldr.baldrDir, "cache")
  val cacheObjectDir = new File(cacheDir, "objects")
  val cacheSaveDir = new File(cacheDir ,"saves")

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
      val checksum = Checksum.getChecksum(file).asInt().toString
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
