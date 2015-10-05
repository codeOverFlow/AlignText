package com.codeoverflow.nlp

/**
 * Created by codeoverflow on 03/10/15.
 */
import com.codeoverflow.models.Aligned
import com.codeoverflow.helpers.FileWriter

object AlignedWriter {

  def toCSVFormat(alignedData: List[Aligned]): String = {
    alignedData.map(x => s"${x.w0};${x.w1}").mkString("\n")
  }

  def writeToCsv(alignedData: List[Aligned], fileName: String = "result.csv") = {
    FileWriter.write(fileName, toCSVFormat(alignedData))
  }
}
