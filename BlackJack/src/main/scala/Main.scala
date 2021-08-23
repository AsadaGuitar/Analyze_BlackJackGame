import blackjack._
import blackjack.system.flow.action.ActionFlow
import blackjack.system.flow.calculation.CalculationFlowAbstract
import blackjack.system.parts.BlackJackIO.readBlackJackHand

import scala.annotation.tailrec
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.language.postfixOps

object Main {

  val useTramps = Seq(1,2,3,4,5,6,7,8,9,10,10,10,10)

  implicit def exchangeStrArray(deck: Deck): Array[String] = deck.map(_.toString).toArray

  implicit val timeOut: FiniteDuration = 10 second


  def initDeck(deckNum: Int, tramps: Seq[Int]): Deck = {
    def addTramps(num: Int, ts: Seq[Int]): Seq[Int] = {
      if (num <= 1) ts
      else ts ++ addTramps(num - 1, ts)
    }
    if (deckNum < 1) Nil
    else addTramps(deckNum * 4, tramps)
  }

  @tailrec
  def main(args: Array[String]): Unit = {

    println("Analyze_BlackJackを開始します。")

    val deck: Deck = args match {
      case x if 0 < x.length => x.map(_.toInt).toSeq
      case x => initDeck(1,useTramps)
    }

    val (userHandA, deckDeletedUserHandA) = readBlackJackHand("ユーザー", deck)
    val (userHandB, deckDeletedUserHandB) = readBlackJackHand("ユーザー", deckDeletedUserHandA)
    val (dealerHand, deckDeletedDealerHand) = readBlackJackHand("ディーラ", deckDeletedUserHandB)
    val userHand = userHandA + userHandB

    val calculator = new CalculationFlowAbstract(userHand,dealerHand,deckDeletedDealerHand)
    val action = calculator.startCalculation()

    val actionFlow = ActionFlow.create(action,userHand,dealerHand,deckDeletedDealerHand)

    val (command,completeDeck) = actionFlow.action()

    command match {
      case Continue => main(completeDeck)
      case Finish => println("shut down Analyze_BlackJack_System...")
    }
  }
}
