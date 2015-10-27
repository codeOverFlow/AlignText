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

  //@todo inversed file
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
           dict: Map[String, List[String]]): mutable.Map[String, List[(String, Double)]] = {
    //val enTxt = Source.fromFile(new File("corpus/termer_target/corpus.lem")).getLines().mkString(" ")
    context.map { case (k, v) =>
      (k, v.filter(sd => dict.contains(sd._1)).flatMap { case (s, d) =>
        //println("d = " + d)
        dict.getOrElse(s, List[String]()).map { str =>
          //val reg = ("""\s""" + str + """\/""").r
          //val sum = (reg findAllIn enTxt).length.toDouble
          //println(s + " = " + dict.getOrElse(s, List("")))
          //println("d/dict(s).length.toDouble = " + d/dict(s).length.toDouble)
          (str, (d / dict(s).length.toDouble) /* * sum*/ )
        }
      }.toList /*.filter { case (s, d) => d > 0.0005 }*/ )
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
        var downLeft = 0.0
        var downRight = 0.0
        val up = v.flatMap { case (s, d) =>
          vv.map { case (ss, dd) =>
            if (s.equalsIgnoreCase(ss))
              dd * dd
            else
              0.0
          }
        }.sum
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
    }.sortWith(_._2 > _._2))
    }
}
