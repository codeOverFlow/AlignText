package com.codeoverflow.nlp

/**
 * Created by codeoverflow on 03/10/15.
 */
import com.codeoverflow.models.Aligned
import com.codeoverflow.helpers.FileWriter

object AlignedWriter {

  def toCSVFormat(alignedData: List[Aligned], separator: String): String = {
    alignedData.map(x => s"${x.w0}${separator}${x.w1}").mkString("\n")
  }

  def writeToCsv(alignedData: List[Aligned], fileName: String = "result.csv", separator: String = ";") = {
    FileWriter.write(fileName, toCSVFormat(alignedData, separator))
  }
}
