package com.codeoverflow

import java.io._

import com.codeoverflow.helpers.{FileWriter, _}
import com.codeoverflow.nlp._

import scala.collection.mutable

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

    FileWriter.write("sources.txt", sources.head.toString.replace(")),", ")),\n"))

    println("Done.\n")


    // And here goes pre-processing!
    println("Pre-processing...")

    val sourcesTerms = Timer.executionTime {
      sources.map( ft => Preprocessing.applyToFileTermer(ft.terms))
    }
    FileWriter.write("sources_pre.txt", sourcesTerms.head.toString().replace(")),", ")),\n"))
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
    /*val (srcMapContext, trgMapContext) = Timer.executionTime {
      (ContextVector.build(sourcesTerms, 3), ContextVector.build(targetTerms, 3))
    }

    println("Done.")

    println("\nSize map: " + srcMapContext.keys.toList.length)
    println("Context cancer: " + srcMapContext("cancer").keys.toList.length)
    println("Done.\n")

    FileWriter.write("context_fr.txt", srcMapContext.toString().replace("),", "),\n"))
    // serialize it
    println("Serialize them in files...")
    val srcOos = new ObjectOutputStream(new FileOutputStream("src_save"))
    srcOos.writeObject(srcMapContext)
    srcOos.close()
    val trgOos = new ObjectOutputStream(new FileOutputStream("trg_save"))
    trgOos.writeObject(trgMapContext)
    trgOos.close()
    println("Done.")*/



    println("Read them from files...")
    val srcOis = new ObjectInputStream(new FileInputStream("src_save"))
    val trgOis = new ObjectInputStream(new FileInputStream("trg_save"))
    val (srcMapContext, trgMapContext) = (srcOis.readObject().asInstanceOf[mutable.Map[String, mutable.Map[String, Double]]],
      trgOis.readObject().asInstanceOf[mutable.Map[String, mutable.Map[String, Double]]])
    srcOis.close()
    trgOis.close()


    println("Traduction du vecteur...")
    /*val srcTradMapContext = Timer.executionTime {
      StandardMethod.trad(srcMapContext, dict)
    }

    FileWriter.write("context_trad_fr.txt", srcTradMapContext.toString().replace(")),", ")),\n"))

    println("Serialize it in files...")
    val srcOos2 = new ObjectOutputStream(new FileOutputStream("src_trad_save"))
    srcOos2.writeObject(srcTradMapContext)
    srcOos2.close()*/


    println("Read it from files...")
    val srcOis2 = new ObjectInputStream(new FileInputStream("src_trad_save"))
    val srcTradMapContext = srcOis2.readObject().asInstanceOf[mutable.Map[String, List[(String, Double)]]]

    println("\nReduction aux mot a traduire...")
    val filteredContext = Timer.executionTime {
      srcTradMapContext.filter { case (s, l) => specializedDict.contains(s) }
    }

    FileWriter.write("context_filtered_fr.txt", filteredContext.toString().replace(")),", ")),\n"))

    println("\nRecherche de candidat...")
    val listOfCandidates = Timer.executionTime {
      StandardMethod.lookForCandidates(filteredContext, trgMapContext.map { case (k, v) => (k, v.map { case (kk, vv) => (kk, vv) }.toList) })
    }

    FileWriter.write("candidates.txt", listOfCandidates.toString().replace(")),", ")),\n"))


    println("\nCalcul de precision...")
    var done = List[String]()
    val accuracy = Timer.executionTime {
      listOfCandidates.flatMap { case (key, value) =>
        value.slice(0, 50).map { case (s, d) =>
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
