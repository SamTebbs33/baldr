package com.github.samtebbs33.baldr

import java.io.File
import java.security.MessageDigest

/**
  * Created by samtebbs on 08/09/2016.
  */
object Checksum {

  def getChecksum(file: File): String = {
    val hexString = new StringBuffer()
    val md = MessageDigest.getInstance("MD5")
    val hash = md.digest()
    hash.foreach(byte => {
      if ((0xff & byte) < 0x10) {
        hexString.append("0"
          + Integer.toHexString((0xFF & byte)));
      } else {
        hexString.append(Integer.toHexString(0xFF & byte));
      }
    })
    hexString.toString
  }

}
