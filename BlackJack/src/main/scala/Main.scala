import blackjack.calculation.statistical.BlackJackStatisticalCreater
import blackjack.{Deck, Hand}

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps


object Main extends App {

  def initDeck(deckNum: Int, tramps: Seq[Int]): Deck = {
    def addTramps(num: Int, ts: Seq[Int]): Seq[Int] = {
      if (num <= 1) ts
      else ts ++ addTramps(num - 1, ts)
    }
    if (deckNum < 1) Nil
    else addTramps(deckNum * 4, tramps)
  }

  val deck = initDeck(1,Seq(1,2,3,4,5,6,7,8,9,10,10,10,10))
  val dHand = new Hand(7)

  val blackJackStatisticalCreater =
    new BlackJackStatisticalCreater(
      dHand,
      deck,
      30 second)

  val start = System.currentTimeMillis()
  val blackJackStatistical = blackJackStatisticalCreater.create()
  val end = System.currentTimeMillis()

  val listA = blackJackStatistical.toList.map(_._2)
  val a = listA.reduceLeft((acc,x) => acc + x)
  val b = blackJackStatistical.toMap

  listA.foreach(x => println(x))
  println("計算結果件数 : " + listA.size)
  println("正確性 : " + a.get().toFloat * 100 + "%")

}
