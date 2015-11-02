package com.codeoverflow.models

/**
 * Created by codeoverflow on 03/10/15.
 */
case class Term(word: String, lemme: String, tag: String)

case class FileTermer(filename: String, terms: List[List[Term]])
case class Aligned(w0: String, w1: String, kind: String)
