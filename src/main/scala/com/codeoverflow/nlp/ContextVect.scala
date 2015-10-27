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

  def build(terms: List[List[List[Term]]], size: Int): mutable.Map[String, mutable.Map[String, Double]] = {
    var myMap = mutable.Map[String, mutable.Map[String, Double]]()
    var inversedMap = mutable.Map[String, List[String]]()

    terms.foreach(_.foreach { t =>
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
    })

    println("Normalisation...")
    var normalized = mutable.Map[String, mutable.Map[String, Double]]()
    myMap.keys.toList.foreach { case s => normalized += (s -> mutable.Map[String, Double]()) }
    // all cooc
    val wStar = myMap.map { case (k, v) =>
      v.map(_._2).sum
    }.sum
    myMap.foreach { case (k, v) =>
      v.foreach { case (kk, vv) =>
        if (vv == 1.0)
          v -= kk
        else {
          val a = vv
          val sumVect = v.map(_._2).sum
          //println("v = " + v)
          //println("sumVect: " + sumVect)
          val b = sumVect - a
          val c = inversedMap.getOrElse(kk, List()).filterNot(_.equalsIgnoreCase(k)).map { x => myMap.getOrElse(x, mutable.Map()).map(_._2).sum }.sum
          val d = wStar - a - b - c
          //println("a: " + a +" | b: " + b + " | c: " + c + " | d: " + d + "\n\n")
          normalized(k) += (kk -> scala.math.log(((a + 0.5) * (d + 0.5)) / ((b + 0.5) * (c + 0.5))))
        }
      }
    }

    println("Remove empty...")
    normalized = normalized.filterNot(_._2.isEmpty)

    normalized
  }
}
