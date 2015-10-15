package com.codeoverflow.nlp

import com.codeoverflow.models.Term

/**
 * Created by codeoverflow on 13/10/15.
 */

/*class ContextVect {
  var contextVect_ = scala.collection.mutable.Map[String, scala.collection.mutable.Map[String, Double]]()
}*/


case class ContextVector(word: String, context: List[String])
case class ContextVectorFrequency(word: String, context: List[(String, Double)])

object ContextVector {
  def build(terms: List[Term], size: Int): List[ContextVector] = {
    terms.zipWithIndex.map { case (e, i) =>
      ContextVector(e.lemme, (terms.slice(i - size, i) ++ terms.slice(i + 1, i + size + 1)).map(_.lemme))
    }
  }

  def addFrequencies(vectors: List[ContextVector]): List[ContextVectorFrequency] = {
    vectors.groupBy(_.word).map { case (k, v) =>
      ContextVectorFrequency(k, v.flatMap(_.context).groupBy(x => x).map { case (kv, vv) =>
        (kv, vv.length.toDouble) }.toList.filter { case (s, f) => f > 1.0 } )
    }.toList.filter(!_.context.isEmpty)
  }

  def toMap(vectors: List[ContextVectorFrequency]): Map[String, List[(String, Double)]] =
    vectors.groupBy(_.word).map { case (k, v) => (k, v.flatMap(_.context)) }
}
