package com.codeoverflow.helpers

/**
 * Author: codeoverflow
 * Date: 12/10/15
 */
object XMLTools {

  /**
   * Load an XML file and make a pretty stucture with it
   * @param file the file name to load
   * @return a pretty structure
   */
  def loadXML(file: String): Map[String, List[String]] = {
    val data = scala.xml.XML.loadFile(file)
    var lemme = ""
    val myStruct = (data \\ "TRAD").filter { e => (e \ "@valid").text == "yes" }.flatMap { e =>
      (e \\ "LANG").map { l =>
        val texte = (l \ "@type").text
        if (texte == "source") {
          lemme = (l \ "LEM").text
        }
        if (!lemme.equalsIgnoreCase((l \ "LEM").text))
          (lemme, (l \ "LEM").text)
        else
          ("", "")
      }
    }.toList.groupBy(_._1).map { case (s, l) => (s, l.map(_._2).distinct) }.filterNot(_._2.head == "")
    FileWriter.write("xml.txt", myStruct.toString().replace("),", "),\n"))
    myStruct
  }
}
