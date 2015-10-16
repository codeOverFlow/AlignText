package com.codeoverflow.helpers

import scala.collection.mutable

/**
 * Created by codeoverflow on 12/10/15.
 */
object XMLTools {

  /**
   * Load an XML file and make a pretty stucture with it
   * @param file the file name to load
   * @return a pretty structure
   */
  def loadXML(file: String): mutable.Map[String, String] = {
    var myStruct = scala.collection.mutable.Map[String, String]()
    val data = scala.xml.XML.loadFile(file)
    var last = ""
    (data \\ "TRAD").filter { e => (e \ "@valid").text == "yes" }.map { e =>
      (e \\ "LANG").map { l =>
        val texte = (l \ "@type").text
        if (texte == "source") {
          last = (l \ "LEM").text
          //myStruct.put((l \ "LEM").text, "")
        }
        else {
          myStruct.put(last, (l \ "LEM").text)
        }
      }
    }
    //println(myStruct)
    myStruct
  }
}
