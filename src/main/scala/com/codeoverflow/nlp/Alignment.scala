package com.codeoverflow.nlp

/**
 * Created by codeoverflow on 03/10/15.
 */
import com.codeoverflow.models._

object Alignment {
  def findCognates(sources: List[Term], targets: List[Term]): List[Aligned] = {
    sources.flatMap(
      x => targets.filter(
        y => isEquivalentTag(x.tag, y.tag) && isCognate(x.lemme, y.lemme)).map(y => Aligned(x.lemme, y.lemme, "cognate"))
    )
  }

  def isCognate(w0: String, w1: String) =
    !isTransfugee(w0, w1) && w0.substring(0, 4) == w1.substring(0, 4) &&
      bigram_feature(w0, w1) > 0.75

  def isTransfugee(w0: String, w1: String) = w0 == w1

  def bigram_feature(w0: String, w1: String): Double = {
    var bi_w0 = List[String]()
    (1 until w0.length).foreach { i =>
      bi_w0 = w0.substring(i - 1, i + 1) :: bi_w0
    }
    var bi_w1 = List[String]()
    (1 until w1.length).foreach { i =>
      bi_w1 = w1.substring(i - 1, i + 1) :: bi_w1
    }
    (2.0 * bi_w0.intersect(bi_w1).length) / bi_w0.union(bi_w1).length
  }

  def isEquivalentTag(t0: String, t1: String): Boolean = {
    t0 match {
      case x if x.startsWith("SBC") && t1.startsWith("NN") => true
      case x if x.startsWith("ADJ") && t1.startsWith("JJ") => true
      case _ => false
    }
  }
}
