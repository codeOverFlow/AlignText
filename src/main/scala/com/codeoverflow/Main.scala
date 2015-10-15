package com.codeoverflow

import java.io.File

import com.codeoverflow.helpers._
import com.codeoverflow.models.Term
import com.codeoverflow.nlp._

/**
 * Created by codeoverflow on 03/10/15.
 */
object Main {
  lazy val dict = ReadDict("dicfrenelda-utf8.txt", 0, 3)
  lazy val specializedDict = XMLTools.loadXML("ts.xml")

  def main(args: Array[String]) {


    val sourceTermsFile = new File("corpus/termer_source/corpus.lem")
    val targetTermsFile = new File("corpus/termer_target/corpus.lem")

    // From unstructured to structured data
    println("Structuring raw data...")

    val sources = Timer.executionTime { TermsExtractor.rawTermerFileToHandyStruct(sourceTermsFile) }
    val targets = Timer.executionTime { TermsExtractor.rawTermerFileToHandyStruct(targetTermsFile) }

    println("Done.\n")

    // And here goes pre-processing!
    println("Pre-processing...")

    val sourcesTerms = Timer.executionTime {
      sources.map( ft => Preprocessing.applyToFileTermer(ft.terms))
    }

    val targetTerms = Timer.executionTime {
      targets.map(ft => Preprocessing.applyToFileTermer(ft.terms))
    }

    println("Done.\n")

    /*
    println("Starting alignment...")

    // Starting alignment process...
    val aligned = Timer.executionTime { Alignment.findCognates(sourcesTerms2, targetsTerms2) }

    println(s"Cognates number: ${aligned.length}")

    // Write results to CSV ;)
    AlignedWriter.writeToCsv(aligned.sortBy(_.w0), "result-cognate.csv", ";;;")
    */

    // Standard method
    println("\nProcess Standard method...")
    println("Vecteurs de contexte...")
    val (srcMapContext, trgMapContext) = Timer.executionTime {
      StandardMethod.createVectors(sourcesTerms, targetTerms)
    }

    println("Size map: " + srcMapContext.keys.toList.length)
    println("Context cancer: " + srcMapContext("cancer").length)
    println("Done.\n")

    FileWriter.write("context_fr.txt", srcMapContext.toString().replace(")),", ")),\n"))

    println("Traduction du vecteur...")
    val srcTradMapContext = Timer.executionTime { StandardMethod.trad(srcMapContext, dict) }

    FileWriter.write("context_trad_fr.txt", srcTradMapContext.toString())

    println("Texte d'existance des mot a traduire...")
    specializedDict.foreach {case (s, ts) =>
      if (!srcTradMapContext.contains(s)) println(s)
    }
  }
}
