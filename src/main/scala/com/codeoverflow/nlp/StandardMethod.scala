package com.codeoverflow.nlp

import com.codeoverflow.models.Term

import scala.collection.mutable

/**
 * Author: codeoverflow
 * Date: 13/10/15
 */

case class CandidateVector(word: String, candidates: List[(String, Double)])


object StandardMethod {
  def countOccurences(l: List[Term], s: String): Map[String, Double] = {
    var done = List[String]()
    l.map { t =>
      if (!done.contains(t.lemme.toLowerCase)) {
        val fileReferenceRegex = ("""\/""" + t.lemme.toLowerCase).r
        done = t.lemme.toLowerCase :: done
        (t.lemme.toLowerCase, fileReferenceRegex.findAllIn(s).length.toDouble)
      }
      else {
        ("", 0.0)
      }
    }.toMap
  }

  def trad(context: mutable.Map[String, mutable.Map[String, Double]],
           dict: Map[String, List[String]],
           cognates: Map[String, List[String]],
           reversedCognates: Map[String, List[String]],
           occurences: Map[String, Double]): mutable.Map[String, List[(String, Double)]] = {
    //val enTxt = Source.fromFile(new File("corpus/termer_target/corpus.lem")).getLines().mkString(" ")
    context.map { case (k, v) =>
      (k, v.filter(sd => dict.contains(sd._1) || cognates.contains(sd._1) || reversedCognates.contains(sd._1)).flatMap { case (s, d) =>
        val sli = dict.getOrElse(s.toLowerCase, List[String]()) ++ cognates.getOrElse(s.toLowerCase, List[String]()) ++ reversedCognates.getOrElse(s.toLowerCase, List[String]())
        sli.map { str =>
          (str.toLowerCase, d * occurences.getOrElse(str.toLowerCase, 0.0) / scala.math.max(sli.map(occurences.getOrElse(_, 0.0)).sum, 1.0))
        }
      }.toList.filterNot { case (s, d) => d == 0.0 })
    }.filter { case (s, l) => l.nonEmpty }
  }


  def lookForCandidates(context: mutable.Map[String, List[(String, Double)]],
                        context_en: mutable.Map[String, List[(String, Double)]]): Map[String, List[(String, Double)]] =
    context.map { case (k, v) =>
      CandidateVector(k, context_en..map { case (kk, vv) =>
        var downLeft = 0.0
        var downRight = 0.0
        val up = v.flatMap { case (s, d) =>
          vv.map { case (ss, dd) =>
            if (s.equalsIgnoreCase(ss)) {
              d * dd
            }
            else {
              0.0
            }
          }
        }.sum
        for (i <- v.indices) {
          downRight += scala.math.pow(v(i)._2, 2)
        }
        downRight = scala.math.sqrt(downRight)
        for (i <- vv.indices) {
          downLeft += scala.math.pow(vv(i)._2, 2)
        }
        downLeft = scala.math.sqrt(downLeft)
        (kk, up / (downLeft * downRight))
      }.toList)
    }.toList.groupBy(_.word).map { case (k, v) => (k, v.flatMap { c =>
      c.candidates.filterNot { case (s, d) => d == 0.0 }.map { case (s, d) => (s, d) }
    }.sortWith(_._2 > _._2))
    }
}
