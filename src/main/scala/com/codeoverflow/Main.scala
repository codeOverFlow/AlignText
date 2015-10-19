package com.codeoverflow

import java.io.File

import com.codeoverflow.helpers._
import com.codeoverflow.nlp._

/**
 * Author: codeoverflow
 * Date: 03/10/15
 */
object Main {
  lazy val dict = ReadDict("dicfrenelda-utf8.txt", 0, 3)
  lazy val dictCognates = ReadDict("result-cognate.csv", 0, 3)
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
    val sourcesTerms2 = Timer.executionTime {
      Preprocessing(sources)
    }

    val targetTerms = Timer.executionTime {
      targets.map(ft => Preprocessing.applyToFileTermer(ft.terms))
    }
    val targetsTerms2 = Timer.executionTime {
      Preprocessing(targets)
    }

    println("Done.\n")


    /*println("Starting alignment...")

    // Starting alignment process...
    val aligned = Timer.executionTime { Alignment.findCognates(sourcesTerms2, targetsTerms2) }

    println(s"Cognates number: ${aligned.length}")

    // Write results to CSV ;)
    AlignedWriter.writeToCsv(aligned.sortBy(_.w0), "result-cognate.csv", ";;;")*/


    // Standard method
    println("\nMethode standard...")
    println("Vecteurs de contexte...")
    // creation des vecteurs
    val (srcMapContext, trgMapContext) = Timer.executionTime {
      (ContextVector.build(sourcesTerms, 3), ContextVector.build(targetTerms, 3))
    }
    //val (srcMapContext, trgMapContext) = Timer.executionTime { StandardMethod.createVectors(sourcesTerms, targetTerms) }


    // normalisation
    /*val (contin_b_src, contin_b_trg)  =
      Timer.executionTime { (StandardMethod.contingency_b(srcMapContext), StandardMethod.contingency_b(trgMapContext)) }*/

    /*val (contin_c_src, contin_c_trg) =
      Timer.executionTime { (StandardMethod.contingency_c(srcMapContext), StandardMethod.contingency_c(trgMapContext)) }

    val (contin_d_src, contin_d_trg) =
      Timer.executionTime { (StandardMethod.contingency_d(srcMapContext), StandardMethod.contingency_d(trgMapContext)) }*/

    /*val (srcCleanedContext, trgCleanedContext) =
      Timer.executionTime { (ContextVector.clean(srcMapContext), ContextVector.clean(trgMapContext)) }*/

    /*val (srcNormalizedContext, trgNormalizedContext) =
      Timer.executionTime { (ContextVector.normalize(srcMapContext, contin_b_src, contin_c_src, contin_d_src),
        ContextVector.normalize(trgMapContext, contin_b_trg, contin_c_trg, contin_d_trg)) }*/


    println("Done.")

    println("\nSize map: " + srcMapContext.keys.toList.length)
    println("Context cancer: " + srcMapContext("cancer").keys.toList.length)
    println("Done.\n")

    FileWriter.write("context_fr.txt", srcMapContext.toString().replace("),", "),\n"))

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
      StandardMethod.lookForCandidates(filteredContext, trgMapContext.map { case (k, v) => (k, v.map { case (kk, vv) => (kk, vv) }.toList) })
    }

    FileWriter.write("candidates.txt", listOfCandidates.toString().replace(")),", ")),\n"))



    println("\nCalcul de precision...")
    var done = List[String]()
    val accuracy = Timer.executionTime {
      listOfCandidates.flatMap { case (key, value) =>
        value.map { case (s, d) =>
          if (!done.contains(key) && specializedDict(key).exists(_.equalsIgnoreCase(s))) {
            println(key + " <==" + specializedDict(key).find(_.equalsIgnoreCase(s)) + "==> " + s)
            done = key :: done
            1
          }
          else
            0
        }
      }.sum
    }

    println("\nPrecision: " + accuracy)

  }
}
