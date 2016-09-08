package com.github.samtebbs33.baldr

import java.io.File
import java.security.MessageDigest

import com.google.common.hash.Hashing
import com.google.common.io.Files

/**
  * Created by samtebbs on 08/09/2016.
  */
object Checksum {

  def getChecksum(file: File) = Files.hash(file, Hashing.crc32())

}
