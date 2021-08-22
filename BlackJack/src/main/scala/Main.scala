import blackjack.Deck
import blackjack.calculation.statistical.BlackJackStatisticalCreater
import blackjack.calculation.statistical.probability.{BlackJackProbs, Rational}
import blackjack.calculation.statistical.probability.strategy.{BasicStrategy, DetailsStrategy}
import blackjack.system.parts.BlackJackIO.{readAction, readBlackJackHand, readDeckNum}

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps


object Main extends App {


  println("Analyze_BlackJackを開始します。")

  val action = mainFlow()

  def initDeck(deckNum: Int, tramps: Seq[Int]): Deck = {
    def addTramps(num: Int, ts: Seq[Int]): Seq[Int] = {
      if (num <= 1) ts
      else ts ++ addTramps(num - 1, ts)
    }
    if (deckNum < 1) Nil
    else addTramps(deckNum * 4, tramps)
  }

  def printResultRational(rational: Option[Rational], text: String): Unit =
    rational match {
      case Some(x) => println(s"$text : ${x.get().toFloat * 100} %")
      case None => println(s"$text : 0 %")
    }

  def mainFlow() ={

    val useTramps = Seq(1,2,3,4,5,6,7,8,9,10,10,10,10)
    val timeOut = 10 second

    val deckNum = readDeckNum()
    val deck = initDeck(deckNum,useTramps)

    val (userHandA, deckDeletedUserHandA) = readBlackJackHand("ユーザー",deck)
    val (userHandB, deckDeletedUserHandB) = readBlackJackHand("ユーザー",deckDeletedUserHandA)

    val (dealerHand, deckDeletedDealerHand) = readBlackJackHand("ディーラ",deckDeletedUserHandB)

    val userHand = userHandA + userHandB


    val statisticalCreate =
      new BlackJackStatisticalCreater(dealerHand,deckDeletedDealerHand,timeOut)

    val action = try{

      println("計算を開始します。")
      val statistical = statisticalCreate.create()
      val probabilityOfBlackJack = new BlackJackProbs(userHand.sum, deck, statistical)

      val strategy = new DetailsStrategy(probabilityOfBlackJack)

      println("詳細計算完了")

      printResultRational(probabilityOfBlackJack.accuracyProb,"正確性")
      printResultRational(probabilityOfBlackJack.dealerBurstProb,"ディーラバースト率")
      printResultRational(probabilityOfBlackJack.dealerWinProb,"ディーラ勝率")
      printResultRational(probabilityOfBlackJack.dealerLoseProb, "ディーラ負率")
      printResultRational(probabilityOfBlackJack.userBurstProb,"ユーザバースト率")
      printResultRational(probabilityOfBlackJack.userWinProb,"ユーザ勝率")

      strategy.nextAction()
    }
    catch {
      case _: Exception =>

        val strategy = new BasicStrategy(userHand,dealerHand)

        println("簡略計算完了")
        strategy.nextAction()
    }

    println(s"計算結果 : ${action}が最善手です。")

    val userAction = readAction()

    userAction
  }
}
