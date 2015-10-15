package com.codeoverflow.nlp

/**
 * Created by codeoverflow on 03/10/15.
 */

import java.io.File

import com.codeoverflow.models.{FileTermer, Term}

import scala.io.Source

object Preprocessing {
  val tagsWeWant = List("ADJ", "JJ", "SBC", "NN")
  val tagsWeIgnore = List("ADJ1", "ADJ2", "NNP")

  def apply(s: List[FileTermer]): List[Term] = {
    s.flatMap(_.terms).groupBy(_.lemme).filter(isOccuredEnougth).flatMap(_._2).filter(isItInteresting).groupBy(_.lemme)
      .map(_._2.head).toList
  }

  def isItInteresting(t: Term): Boolean = {
    t.lemme.length > 2
  }

  def isOccuredEnougth(tuple: (String, List[Term])): Boolean = {
    tuple._2.length > 1
  }

  def applyToFileTermer(s: List[Term]): List[Term] = {
    val toolwords =  Source.fromFile(new File("toolwords.txt")).getLines().toList
    val stop =  Source.fromFile(new File("stop.txt")).getLines().toList
    s.filter(isItInteresting).filter(t => !toolwords.contains(t.word)).filter(t => !stop.contains(t.word))
  }
}