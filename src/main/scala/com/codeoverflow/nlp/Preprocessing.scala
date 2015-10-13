package com.codeoverflow.nlp

/**
 * Created by codeoverflow on 03/10/15.
 */
import com.codeoverflow.models.{FileTermer, Term}

object Preprocessing {
  //val tagsWeWant = List("ADJ", "JJ", "SBC", "NN")
  val tagsWeIgnore = List("DTN","DTC","INJ","PUL","PFX","PREP","PRV","PRO","REL","SUB","CC","DT","EX","IN","LS","MD",
    "PDT","POS","PP","RP","TO","UH","WP")

  def apply(s: List[FileTermer]): List[Term] = {
    s.flatMap(_.terms).groupBy(_.lemme).filter(isOccuredEnougth).flatMap(_._2).filter(isItInteresting).groupBy(_.lemme)
      .map(_._2.head).toList
  }

  def isItInteresting(t: Term): Boolean = {
    t.lemme.length > 4 && !tagsWeIgnore.exists(x => t.tag.startsWith(x))
  }

  def isOccuredEnougth(tuple: (String, List[Term])): Boolean = {
    tuple._2.length > 5
  }

  def applyToFileTermer(s: List[Term]): List[Term] = {
    s.filter(isItInteresting)
  }
}