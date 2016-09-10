package com.github.samtebbs33.baldr

import java.io.{File, FileOutputStream}
import java.util.zip.ZipOutputStream

import scala.collection.mutable

/**
  * Created by samtebbs on 09/09/2016.
  */
class Save(hash: String) {

  val cacheInterval = 5
  val metaAttributes = new mutable.HashMap[String, String]()

  def addMetaAttribute(name: String, value: String) = metaAttributes.put(name, value)

  def write(files: Array[File]): Unit = {
    if(Branch.current.savesSinceCache >= cacheInterval) {
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
  val savesDir = new File(Baldr.baldrDir.getAbsolutePath, "saves")
}
