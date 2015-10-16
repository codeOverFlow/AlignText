package com.codeoverflow

import java.io.File

import com.codeoverflow.helpers._
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
    println("\nMethode standard...")
    println("Vecteurs de contexte...")
    val (srcMapContext, trgMapContext) = Timer.executionTime {
      StandardMethod.createVectors(sourcesTerms, targetTerms)
    }

    println("\nSize map: " + srcMapContext.keys.toList.length)
    println("Context cancer: " + srcMapContext("cancer").length)
    println("Done.\n")

    FileWriter.write("context_fr.txt", srcMapContext.toString().replace(")),", ")),\n"))

    println("Traduction du vecteur...")
    val srcTradMapContext = Timer.executionTime {
      StandardMethod.trad(srcMapContext, trgMapContext, dict)
    }

    FileWriter.write("context_trad_fr.txt", srcTradMapContext.toString().replace(")),", ")),\n"))

    println("\nReduction aux mot a traduire...")
    val filteredContext = Timer.executionTime {
      srcTradMapContext.filter { case (s, l) => specializedDict.contains(s) }
    }

    println("\nRecherche de candidat...")
    val listOfCandidates = Timer.executionTime {
      StandardMethod.lookForCandidates(filteredContext, trgMapContext)
    }

    FileWriter.write("candidates.txt", listOfCandidates.toString().replace(")),", ")),\n"))



    println("\nCalcul de precision...")
    var done = List[String]()
    val accuracy = Timer.executionTime {
      listOfCandidates.flatMap { case (key, value) =>
        value.slice(0, 10).map { case (s, d) =>
          if (!done.contains(key) && specializedDict(key).equalsIgnoreCase(s)) {
            println(key + " <==" + specializedDict(key) + "==> " + s)
            done = key :: done
            1
          }
          else
            0
        }
      }.foldLeft(0)(_ + _)
    }

    println("\nPrecision: " + accuracy)

  }
}
