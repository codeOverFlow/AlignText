package com.codeoverflow.helpers

import java.io.File

import scala.io.Source

/**
 * Created by codeoverflow on 13/10/15.
 */

object ReadDict {
  def apply(filename: String, iSrc: Int, iTrg: Int): Map[String, List[String]] = {
    val lines = Source.fromFile(new File(filename)).getLines().toList
    lines.map { l =>
      val datas = l.split(";")
      (datas(iSrc), datas(iTrg))
    }.groupBy(_._1).map { case (k,v) => (k, v.map(_._2)) }
  }
}
