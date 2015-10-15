package com.codeoverflow.nlp

import java.io.File

import com.codeoverflow.helpers.{Timer, ReadDict}
import com.codeoverflow.models._

/**
 * Created by codeoverflow on 13/10/15.
 */
object StandardMethod {
  def createVectors(sourcesTerms: List[List[Term]],
                    targetTerms: List[List[Term]]): (Map[String, List[(String, Double)]], Map[String, List[(String, Double)]]) = {
    val (srcVect, trgVect) =
      (sourcesTerms.map(ContextVector.build(_, 3)), targetTerms.map(ContextVector.build(_, 3)))

    val (srcContextWithFrequencies, trgContextWithFrequencies) =
      (ContextVector.addFrequencies(srcVect.flatten), ContextVector.addFrequencies(trgVect.flatten))

    (ContextVector.toMap(srcContextWithFrequencies), ContextVector.toMap(trgContextWithFrequencies))
  }

  def trad(context: Map[String, List[(String, Double)]], dict: Map[String, List[String]]): Map[String, List[(String, Double)]] =
    context.map { case (k, v) =>
      (k, v.filter(sd => dict.contains(sd._1)).flatMap { case (s, d) =>
        dict(s).map((_, d/dict(s).length))
      })
    }

}
