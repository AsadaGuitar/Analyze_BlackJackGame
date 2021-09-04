package com.analysis.common.calculation

import com.analysis.common.calculation._
import com.analysis.common.calculation.Probs

/*
クラス名      ProbsMaker[I <: Iterable[(K, Rational)], K]
機能　　      Iterableを上限境界としたオブジェクトをProbsに変換
引数          l: I
*/
implicit class ProbsMaker[I <: Iterable[(K, Rational)], K](l: I){
  
  /*
  メソッド名     toProbs
  機能          Iterableを上限境界としたオブジェクトをProbsに変換
  戻値          Probs[K]      Probsクラス
  */
  def toProbs: Probs[K] = {
    l.foldLeft(Probs())((acc,x) => acc.addValue(x))
  }
}
