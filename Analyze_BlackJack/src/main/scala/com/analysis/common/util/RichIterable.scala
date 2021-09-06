package com.analysis.common.util

object RichIterable {

  def productOfList(list: List[Int]): Long = {
    //先頭要素から各要素の積を求める
    def loop(ints: List[Int]): Long = ints match {
      case Nil => 1
      case head :: tail => head * loop(tail)
    }
    //リストが空の場合、1を返却
    if (list.isEmpty) 1
    else loop(list)
  }

  /*
  トレイト名     RichDiff
  機能          リストオブジェクトに
  */
  trait RichDiff[I <: Seq[T], T]{
    def :- (that: T): I
    def :- (that: I): I
  }

  implicit class SequenceDefine[T] (l: Seq[T]) extends RichDiff [Seq[T],T]{
    override def :-(that: T): Seq[T] = l.diff(Seq(that))
    override def :-(that: Seq[T]): Seq[T] = l.diff(that)}
  
  implicit class VectorDefine[T] (l: Vector[T]) extends RichDiff [Vector[T],T]{
    override def :-(that: T): Vector[T] = l.diff(Vector(that))
    override def :-(that: Vector[T]): Vector[T] = l.diff(that)}
  
  implicit class ListDefine[T] (l: List[T]) extends RichDiff [List[T],T]{
    override def :-(that: T): List[T] = l.diff(List(that))
    override def :-(that: List[T]): List[T] = l.diff(that)}
  
}
