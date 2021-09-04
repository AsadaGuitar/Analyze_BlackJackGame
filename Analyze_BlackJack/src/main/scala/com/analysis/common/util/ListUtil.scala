package com.analysis.common.util

object ListUtil {

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
}