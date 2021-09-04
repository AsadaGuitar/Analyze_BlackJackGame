package com.analysis.common.util


private trait RichDiff[I <: Seq[T], T]{
  def :- (that: T): I
}

object RichDiffDefine {
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
