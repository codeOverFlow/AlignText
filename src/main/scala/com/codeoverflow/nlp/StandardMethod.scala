package com.codeoverflow.nlp

import com.codeoverflow.models._

/**
 * Created by codeoverflow on 13/10/15.
 */
object StandardMethod {
  def applyWindow(listTerm: List[List[Term]], ws: Int): ContextVect = {
    var context = new ContextVect

    listTerm.foreach(window(_, ws, context))

    // normalize by the sum of all context frequencies
    context.contextVect_.keys.foreach { k =>
      context.contextVect_(k).keys.foreach { kk =>
        context.contextVect_(k)(kk) = Math.abs(Math.log(context.contextVect_(k)(kk) /
          context.contextVect_(kk).values.toList.foldLeft(0.0)(_ + _)))
      }
    }

    context
  }

  private def window(terms: List[Term], ws: Int, context: ContextVect): Unit = {
    for (i <- 0 to terms.length - 1) {
      val tAtI = terms(i)
      if (i - ws >= 0 && i + ws < terms.length) {
        //println(terms.slice(i - ws, i + ws + 1))
        terms.slice(i - ws, i + ws + 1).map(updateContext(_, tAtI, context))
      }
      else if (i - ws < 0 && i + ws < terms.length) {
        //println(terms.slice(0, i + ws + 1))
        terms.slice(i - ws, i + ws + 1).map(updateContext(_, tAtI, context))
      }
      else if (i - ws >= 0 && i + ws >= terms.length) {
        //println(terms.slice(i - ws, terms.length))
        terms.slice(i - ws, terms.length).map(updateContext(_, tAtI, context))
      }
    }
  }

  private def updateContext(t: Term, pivot: Term, context: ContextVect): Unit = {
    if (t == pivot) {}
    else {
      if (context.contextVect_.contains(pivot.lemme)) {
        if (context.contextVect_(pivot.lemme).contains(t.lemme))
          context.contextVect_(pivot.lemme)(t.lemme) += 1
        else
          context.contextVect_(pivot.lemme) += (t.lemme -> 1)
      }
      else {
        context.contextVect_.put(pivot.lemme, scala.collection.mutable.Map[String, Double]())
        context.contextVect_(pivot.lemme) += (t.lemme -> 1)
      }
    }
  }
}
