package com.codeoverflow.nlp

import scala.collection.mutable

/**
 * Author: codeoverflow
 * Date: 13/10/15
 */

case class CandidateVector(word: String, candidates: List[(String, Double)])


object StandardMethod {
  /*def createVectors(sourcesTerms: List[List[Term]],
                    targetTerms: List[List[Term]]): (Map[String, List[(String, Double)]], Map[String, List[(String, Double)]]) =
    (ContextVector.toMap(ContextVector.addFrequencies(sourcesTerms.map(ContextVector.build(_, 3)).flatten)),
      ContextVector.toMap(ContextVector.addFrequencies(targetTerms.map(ContextVector.build(_, 3)).flatten)))*/


  /*def inversedFile(context: Map[String, List[(String, Double)]]): Map[String, List[String]] = {
    context.flatMap { case (k, v) =>
      v.map { case (s, d) =>
        (s, k)
      }
    }.toList.groupBy(_._1).map { case (s, ss) => (s, ss.map(_._2)) }
  }*/

  /*def contingency_b(context: Map[String, List[(String, Double)]]): Map[(String, String), Double] = {
    context.map { case (k, v) =>
      //println(k + " -> " + v)
      v.map { case (s, d) =>
        ((k,s) , context.keys.toList.length.toDouble - context(k).length.toDouble)
      }
    }.flatten.toMap[(String,String), Double]
  }*/

  def trad(context: mutable.Map[String, mutable.Map[String, Double]],
           context_en: mutable.Map[String, mutable.Map[String, Double]],
           dict: Map[String, List[String]]): mutable.Map[String, List[(String, Double)]] = {
    context.map { case (k, v) =>
      (k, v.filter(sd => dict.contains(sd._1)).flatMap { case (s, d) =>
        dict.getOrElse(s, List[String]()).map { str =>
          var sum = 0.0
          if (context_en.contains(str))
            sum = context_en(str).foldLeft(0.0) { (acc, c0) => acc + c0._2 }
          (str, (d / dict.getOrElse(s, List[String]("")).length.toDouble) /* * (sum / 6.0)*/ )
        }
      }.toList.filter { case (s, d) => d > 0.0005 })
    }.filter { case (s, l) => l.nonEmpty }
  }


  def lookForCandidates(context: mutable.Map[String, List[(String, Double)]],
                        context_en: mutable.Map[String, List[(String, Double)]]): Map[String, List[(String, Double)]] =
  /*context.map { case (k, v) =>
    CandidateVector(k, context_en.map { case (kk, vv) =>
      (kk, v.map(_._1).intersect(vv.map(_._1)).length.toDouble / v.union(vv).length.toDouble)
    }.toList)
  }.toList.groupBy(_.word).map { case (k, v) => (k, v.flatMap { c =>
    c.candidates.map { case (s, d) => (s, d) }
  }.sortWith(_._2 > _._2).filterNot(x => x._2 == 0.0))
  }*/
    context.map { case (k, v) =>
      CandidateVector(k, context_en.map { case (kk, vv) =>
        var up = 0.0
        var downLeft = 0.0
        var downRight = 0.0
        for (i <- 0 to scala.math.min(v.length, vv.length) - 1) {
          up += v(i)._2 * vv(i)._2
        }
        for (i <- 0 to v.length - 1) {
          downRight += v(i)._2
        }
        downRight = scala.math.sqrt(scala.math.pow(downRight, 2))
        for (i <- 0 to vv.length - 1) {
          downLeft += vv(i)._2
        }
        downLeft = scala.math.sqrt(scala.math.pow(downLeft, 2))
        (kk, up / (downLeft * downRight))
      }.toList)
    }.toList.groupBy(_.word).map { case (k, v) => (k, v.flatMap { c =>
      c.candidates.map { case (s, d) => (s, d) }
    }.sortWith(_._2 < _._2).filterNot(x => x._2 == 0.0))
    }
}
