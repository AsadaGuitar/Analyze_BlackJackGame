package com.analysis.common.calculation

import com.analysis.common.Probs

import scala.collection.immutable

/*
オブジェクト名     ProbabilityStatistics
機能             ProbabilityStatisticsクラスのコンパニオンオブジェクト
*/
object Probs {

  /*
  メソッド名     apply
  機能          リストを受けとリ、重複分の値を加算した確率統計を返却
  引数          elems: I <: Seq[(K,Rational)]    Seqを上限境界とした値と確率のタプルリスト
  戻値          重複分の値を加算した確率統計
  */
  def apply[K](elems: (K,Rational)*): Probs[K] = {
    elems.foldLeft(Nil: Probs[K])((acc,x) => acc.addValue(x))
  }

  /*
  メソッド名     apply
  機能          ProbabilityStatisticsの要素を受取り、新しいProbabilityStatisticsを返却
  引数          elems: Map[K,Rational]    ProbabilityStatisticsの要素
  戻値          ProbabilityStatistics[K]  新しいProbabilityStatistics
  */
  private def apply[K](elems: Map[K,Rational]): ProbabilityStatistics[K] = {
    new ProbabilityStatistics(elems)
  }
}

/*
クラス名      ProbabilityStatistics   
機能         確率統計を扱う為のクラス
引数         elems: Map[K,Rational]   値と確率のマップ
*/
final class Probs[K] private (elems: Map[K,Rational]) extends immutable.Map[K,Rational] {

  /*
  メソッド名   this    
  機能        引数無のコンストラクタ
  */
  def this() = this(Map())

  /*
  メソッド名   toString
  機能        文字列に変換した際の表示を設定
  戻値        String      文字列に変換したProbabilityStatistics
  */
  override def toString(): String = elems.mkString("ProbabilityStatistics(", ",", ")")

  /*
  メソッド名   iterator
  機能        foreachを有効にし、リストが持つメソッドを使用可能にする
  戻値        Iterator[(K,Rational)]    反復可能なシーケンス
  */
  override def iterator: Iterator[(K,Rational)] = elems.iterator

  /*
  メソッド名   removed
  機能        要素を削除 
  引数        key: K                  削除する要素のkey
  戻値        ProbablityStatistics    選択した要素を削除した確率統計
  */
  override def removed(key: K): Probs[K] =
    Probs(elems.removed(key))

  /*
  メソッド名   updated
  機能        要素の更新
  引数        key: K                    更新する要素のkey
    　　      value: V1                 更新する値
  戻値        ProbabilityStatistics     値を更新した確率統計
  */
  override def updated[V1 >: Rational](key: K, value: V1) = elems.updated(key,value)

  /*
  メソッド名   get
  機能        keyから確率を取得
  引数        key: K              確率のkey
  戻値        Option[Rational]    取得した確率
  */
  override def get(key: K): Option[Rational] = elems.get(key)

  /*
  メソッド名   get
  機能        条件に合致するkeyの確率の和を取得
  引数        filter: K => Boolean      取得する確率の条件
  戻値        Option[Rational]          条件に合致するkeyの確率の和
  */
  def get(filter: K => Boolean): Option[Rational] = {
    //条件に合致する確率を取得
    val rationals = this.filter(x => filter(x._1)).values

    if (rationals.isEmpty) None
    //条件に合致する確率の和を返却
    else Some(rationals.reduceLeft((acc, x) => acc + x))
  }

  /*
  メソッド名   addValue
  機能        受取った確率統計の値が存在した場合、値を追加。存在しない場合、新しく追加
  引数        key: K                    追加する確率のkey
  　　        value: Rational           追加する確率
  戻値        ProbabilityStatistics     追加した確率統計
  */
  def addValue(key: K, value: Rational): ProbabilityStatistics[K] =
    key match {
      case k if this.contains(k) => Probs(this.updated(key, value + this(k)))
      case _ => Probs(this.updated(key,value))
    }

  /*
  メソッド名   addValue
  機能        受取った確率統計の値が存在した場合、値を追加。存在しない場合、新しく追加
  引数        elem: (K,Rational)        追加する確率のkeyと追加する確率のタプル
  戻値        ProbabilityStatistics     追加した確率統計
  */
  def addValue(elem: (K,Rational)): Probs[K] = addValue(elem._1,elem._2)

  /*
  メソッド名   addValue
  機能        受取った確率統計の値が存在した場合、値を追加。存在しない場合、新しく追加
  引数        elems: Seq[(K,Rational)]    追加する確率のkeyと追加する確率のタプルのリスト
  戻値        ProbabilityStatistics       追加した確率統計
  */
  def addValue(elems: Seq[(K,Rational)]): Probs[K] = elems.foldLeft(this)((acc,x) => acc.addValue(x))
}

