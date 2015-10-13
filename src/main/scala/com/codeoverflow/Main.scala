package com.codeoverflow

import java.io.File

import com.codeoverflow.helpers._
import com.codeoverflow.models.Term
import com.codeoverflow.nlp._

/**
 * Created by codeoverflow on 03/10/15.
 */
object Main {
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

      //val sourcesTerms = Timer.executionTime { Preprocessing(sources) }
      var sourcesTerms: List[List[Term]] = List[List[Term]]()
      Timer.executionTime {
        sources.foreach(ft => sourcesTerms = Preprocessing.applyToFileTermer(ft.terms) :: sourcesTerms)
      }

      var targetTerms: List[List[Term]] = List[List[Term]]()
      Timer.executionTime {
        targets.foreach(ft => targetTerms = Preprocessing.applyToFileTermer(ft.terms) :: targetTerms)
      }

      println(s"Source terms number: ${sourcesTerms.flatMap(_).length}")
      println(s"Target terms number: ${targetTerms.flatMap(_).length}")

      println("Done.\n")

      /*
      println("Starting alignment...")

      // Starting alignment process...
      val aligned = Timer.executionTime { Alignment.findCognates(sourcesTerms, targetsTerms) }

      println(s"Cognates number: ${aligned.length}")

      // Write results to CSV ;)
      AlignedWriter.writeToCsv(aligned.sortBy(_.w0), "result-cognate.csv")
      */

      println("\nProcess XML dictionary...")
      Timer.executionTime {
        XMLTools.loadXML("ts.xml")
      }

      println("\nProcess Standard method...")
      val context_fr = Timer.executionTime {
        StandardMethod.applyWindow(sourcesTerms, 2)
      }
      val context_en = Timer.executionTime {
        StandardMethod.applyWindow(targetTerms, 2)
      }
    }
}
