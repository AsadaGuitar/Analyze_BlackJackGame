package com.analysis.common.util


implicit class RichAnalysisSeq[I <: Seq[A], A](l: I) {
  
  def :-(that: A): I = l.diff(Seq(that))

  def foldProduct: BigInt = {
    //先頭要素から各要素の積を求める
    def loop(ints: I): BigInt = ints match {
      case Nil => 1
      case head :: tail => head * loop(tail)
    }
    //リストが空の場合、1を返却
    if (l.isEmpty) 1
    else loop(l)
  }
}

