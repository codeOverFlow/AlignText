package com.codeoverflow

import com.codeoverflow.nlp._
import com.codeoverflow.helpers._

import java.io.File

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

      val sourcesTerms = Timer.executionTime { Preprocessing(sources) }
      val targetsTerms = Timer.executionTime { Preprocessing(targets) }

      println(s"Source terms number: ${sourcesTerms.length}")
      println(s"Target terms number: ${targetsTerms.length}")

      println("Done.\n")

      println("Starting alignment...")

      // Starting alignment process...
      val aligned = Timer.executionTime { Alignment.findCognates(sourcesTerms, targetsTerms) }

      println(s"Cognates number: ${aligned.length}")

      // Write results to CSV ;)
      AlignedWriter.writeToCsv(aligned.sortBy(_.w0), "result-cognate.csv")
    }
}
