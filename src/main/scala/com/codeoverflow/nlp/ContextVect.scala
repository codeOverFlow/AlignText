package com.codeoverflow.nlp

import com.codeoverflow.models.Term

import scala.collection.mutable

/**
 * Author: codeoverflow
 * Date: 13/10/15
 */

/*class ContextVect {
  var contextVect_ = scala.collection.mutable.Map[String, scala.collection.mutable.Map[String, Double]]()
}*/


case class ContextVector(word: String, context: List[String])
case class ContextVectorFrequency(word: String, context: List[(String, Double)])

object ContextVector {
  /*def build(terms: List[Term], size: Int): List[ContextVector] = {
    terms.zipWithIndex.map { case (e, i) =>
      ContextVector(e.lemme, (terms.slice(i - size, i) ++ terms.slice(i + 1, i + size + 1)).map(_.lemme))
    }
  }

  // TODO: normalize the frequency with the contingency table
  def addFrequencies(vectors: List[ContextVector]): List[ContextVectorFrequency] = {
    vectors.groupBy(_.word).map { case (k, v) =>
      ContextVectorFrequency(k, v.flatMap(_.context).groupBy(x => x).map { case (kv, vv) =>
        (kv, vv.length.toDouble) }.toList.filter(_._2 > 1.0))
    }
  }.toList.filter(_.context.nonEmpty)

  def toMap(vectors: List[ContextVectorFrequency]): Map[String, List[(String, Double)]] =
    vectors.groupBy(_.word).map { case (k, v) => (k, v.flatMap(_.context)) }*/

  def build(terms: List[List[Term]], size: Int): mutable.Map[String, mutable.Map[String, Double]] = {
    // map of context for each keys k
    var myMap = mutable.Map[String, mutable.Map[String, Double]]()
    // give us all the ki which have K in their context
    var inversedMap = mutable.Map[String, List[String]]()

    terms.foreach { t =>
      val lemmeList = t.map(_.lemme)
      lemmeList.zipWithIndex.foreach { case (te, i) =>
        if (myMap.contains(te)) {
          (t.slice(i - size, i) ++ t.slice(i + 1, i + size + 1)).map(_.lemme).foreach { x =>
            if (inversedMap.contains(x)) {
              if (!inversedMap(x).contains(te))
                inversedMap(x) ++= List(te)
            }
            else {
              inversedMap += (x -> List(te))
            }
            if (myMap(te).contains(x))
              myMap(te)(x) += 1.0
            else
              myMap(te) += (x -> 1.0)
          }
        }
        else {
          (t.slice(i - size, i) ++ t.slice(i + 1, i + size + 1)).map(_.lemme).foreach { x =>
            if (inversedMap.contains(x)) {
              if (!inversedMap(x).contains(te))
                inversedMap(x) ++= List(te)
            }
            else {
              inversedMap += (x -> List(te))
            }
            myMap += (te -> mutable.Map[String, Double]())
            if (myMap(te).contains(x))
              myMap(te)(x) += 1.0
            else
              myMap(te) += (x -> 1.0)
          }
        }
      }
    }

    // all cooc
    val wStar = myMap.map { case (k, v) =>
      v.map(_._2).sum
    }.sum
    println("Normalisation...")
    myMap.foreach { case (k, v) =>
      val sumVect = v.map(_._2).sum
      v.foreach { case (kk, vv) =>
        if (vv == 1.0)
          v -= kk
        else {
          // c(i,j) = size of the context of i
          val a = vv
          // c(i, -j) = all words minus ones in i
          val b = sumVect - a
          // c(-i, j) = j in context of -i
          val c = inversedMap(kk).filterNot(_.equalsIgnoreCase(k)).map { x =>
            myMap(x).map(_._2).sum
          }.sum
          // c(-i,-j) = only in context of i
          val d = wStar - a - b - c
          myMap(k)(kk) = scala.math.abs(scala.math.log(((a + 0.5) * (d + 0.5)) / ((b + 0.5) * (c + 0.5))))
        }
      }
      if (v.isEmpty)
        myMap -= k
    }

    myMap
  }
}
