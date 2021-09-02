package com.analysis.blackJack.calculation

import com.analysis.blackJack.{Deck, Hand}
import com.analysis.common.*
import com.analysis.common.calculation.Rational

/*
オブジェクト名　  HandProbabilityCalculator
機能            HandProbabilityCalculatorクラスのコンパニオンオブジェクト
*/
object HandProbabilityCalculator {
  
}

/*
トレイト名   HandProbabilityCalculator
機能        引く可能性のある手札の確率を取得
*/
trait HandProbabilityCalculator {
  import HandProbabilityCalculator._

  /*
  メソッド名     calculateProb
  機能          山札と手札の長さから確率を取得
  引数          hand: Hand      引く可能性のある手札
  　　          deck: Deck      使用中の山札
  戻値          Rational        確率
  */
  def calculateProb(hand: Hand,deck: Deck): Rational = {
    val r: List[Int] = (((deck.length - hand.length) + 2) to deck.length).toList
    new Rational(1, productOfList(r).toLong)
  }
}
