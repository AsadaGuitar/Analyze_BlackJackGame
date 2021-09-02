package blackJack.calculation.probabilityStatistics

import blackJack.{Deck, Hand}
import blackJack.calculation.Rational

/*
オブジェクト名　  HandProbabilityCalculator
機能            HandProbabilityCalculatorクラスのコンパニオンオブジェクト
*/
object HandProbabilityCalculator {

  /*
  メソッド名     productOfList
  機能          リストの要素全ての積を返却
  引数          ints: List[Int]     積を求めるリスト
  戻値          BigInt              リストの要素全ての積
  */
  def productOfList(ints: List[Int]): BigInt = {
    //先頭要素から各要素の積を求める
    def loop(ints: List[Int]): BigInt = ints match {
      case Nil => 1
      case head :: tail => head * loop(tail)
    }
    //リストが空の場合、1を返却
    if (ints.isEmpty) 1
    else loop(ints)
  }
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
