package com.codeoverflow.nlp

import com.codeoverflow.models._

/**
 * Created by codeoverflow on 13/10/15.
 */

case class CandidateVector(word: String, candidates: List[(String, Double)])


object StandardMethod {
  def createVectors(sourcesTerms: List[List[Term]],
                    targetTerms: List[List[Term]]): (Map[String, List[(String, Double)]], Map[String, List[(String, Double)]]) =
    (ContextVector.toMap(ContextVector.addFrequencies(sourcesTerms.map(ContextVector.build(_, 3)).flatten)),
      ContextVector.toMap(ContextVector.addFrequencies(targetTerms.map(ContextVector.build(_, 3)).flatten)))


  def trad(context: Map[String, List[(String, Double)]],
           context_en: Map[String, List[(String, Double)]],
           dict: Map[String, List[String]]): Map[String, List[(String, Double)]] =
    context.map { case (k, v) =>
      (k, v.filter(sd => dict.contains(sd._1)).flatMap { case (s, d) =>
        dict(s).map { str =>
          var sum = 0.0
          if (context_en.contains(str))
            sum = context_en(str).foldLeft(0.0) { (acc, c0) => acc + c0._2 }
          (str, (d / dict(s).length) * (sum / 6.0))
        }
      }.filter { case (s, d) => d > 0.0 })
    }.filter { case (s, l) => !l.isEmpty }


  def lookForCandidates(context: Map[String, List[(String, Double)]],
                        context_en: Map[String, List[(String, Double)]]): Map[String, List[(String, Double)]] =
    context.map { case (k, v) =>
      CandidateVector(k, context_en.map { case (kk, vv) =>
        (kk, v.map(_._1).intersect(vv.map(_._1)).length.toDouble / v.union(vv).length.toDouble)
      }.toList)
    }.toList.groupBy(_.word).map { case (k, v) => (k, v.flatMap { c =>
      c.candidates.map { case (s, d) => (s, d) }
    }.sortWith(_._2 > _._2).filterNot(x => x._2 == 0.0))
    }

}
