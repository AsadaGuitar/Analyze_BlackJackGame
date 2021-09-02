package com.analysis.common.calculation

import com.analysis.common.calculation._

implicit class ProbsMaker[I <: Iterable[(K, Rational)], K](l: I){
  override def toProbs: Probs[K] = {
    l.foldLeft(Nil: Probs[K])((acc,x) => acc.addValue(x))
  }
}