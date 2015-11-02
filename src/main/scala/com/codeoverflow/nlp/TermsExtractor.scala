package com.codeoverflow.nlp

/**
 * Created by codeoverflow on 03/10/15.
 */
import java.io.File

import com.codeoverflow.models._

import scala.io.Source

object TermsExtractor {

  /**
   * Transform a raw terms file to a pretty and handy structure
   *
   * @param raw File containing the raw terms
   *
   * @return List of sexy structure
   */
  def rawTermerFileToHandyStruct(raw: File): List[FileTermer] = {
    val fileContentByFile = splitRawTermerFileContentByFile(Source.fromFile(raw).getLines().toList)
    val termRegex = """([^0-9\s]+)/([^\s]+)/([^0-9\s]+)""".r

    fileContentByFile.keys.toList.map { fileName =>
      var termsByLines = List[List[Term]]()
      fileContentByFile(fileName).foreach { str =>
        val matches = termRegex findAllIn str
        val terms = matches.map(_ =>
          if (matches group 3 contains ":") {
            //println(matches group 3)
            Term(matches group 1, matches.group(3).split(":")(0).replaceAll( """\/""", ""), matches group 2)
          }
          else
            Term(matches group 1, matches.group(3).replaceAll( """\/""", ""), matches group 2)
        ).toList
        termsByLines = termsByLines ++ List(terms)
      }

      FileTermer(fileName, termsByLines)
    }
  }

  /**
   * Split raw terms file content by file
   *
   * @param content Raw terms content line by line
   *
   * @return A map, where key is the file name, and the value is the text content
   */
  def splitRawTermerFileContentByFile(content: List[String]): Map[String, List[String]] = {
    val fileReferenceRegex = """^__FILE=(.*?.txt).*$""".r

    var fileContentByFile = scala.collection.mutable.Map[String, List[String]]()
    var currentFileContent = List[String]()
    var currentFileName = ""

    content.foreach { l =>
      if (l.startsWith("__FILE")) {

        l match {
          case fileReferenceRegex(fileName) => currentFileName = fileName
        }
      }
      else if (l.startsWith("__END")) {
        fileContentByFile += currentFileName -> currentFileContent
        currentFileContent = List[String]()
      }
      else {
        currentFileContent = currentFileContent ++ List(l)
      }
    }
    fileContentByFile.toMap
  }
}
