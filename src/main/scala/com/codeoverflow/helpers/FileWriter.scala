package com.codeoverflow.helpers

/**
 * Created by codeoverflow on 03/10/15.
 */
import java.io.PrintWriter

object FileWriter {

  /**
   * Write to disk
   *
   * @param fileName File's name
   * @param data Data to put in the file
   */
  def write(fileName: String, data: String) = {
    new PrintWriter(fileName) { write(data); close() }
  }
}