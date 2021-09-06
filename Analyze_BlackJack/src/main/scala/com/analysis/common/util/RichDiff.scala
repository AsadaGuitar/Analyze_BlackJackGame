package com.analysis.common.util

/*
トレイト名　　　RichDiff
機能　　　　　　diffメソッドの簡略表現
*/
private trait RichDiff[I <: Seq[T], T]{
  
  /*
  メソッド名
  機能
  引数
  戻値
  */
  def :- (that: T): I
}

/*
オブジェクト名
機能
*/
object RichDiffDefine {
  
  /*
  クラス名
  機能
  引数
  */
  implicit class SequenceDefine[T] (l: Seq[T]) extends RichDiff [Seq[T],T]{
    override def :-(that: T): Seq[T] = l.diff(Seq(that))
  }
  implicit class VectorDefine[T] (l: Vector[T]) extends RichDiff [Vector[T],T]{
    override def :-(that: T): Vector[T] = l.diff(Vector(that))
  }
  implicit class ListDefine[T] (l: List[T]) extends RichDiff [List[T],T]{
    override def :-(that: T): List[T] = l.diff(List(that))
  }
}
