package blackjack.system.parts

import blackjack.calculation.statistical.BlackJackStatisticalCreater
import blackjack.calculation.statistical.probability.BlackJackProbs
import blackjack.calculation.statistical.probability.strategy.{BasicStrategy, DetailsStrategy}
import blackjack.{Action, Deck, Hand}

import scala.concurrent.duration.Duration

object CalculateAction {

  def calculate(userHand: Hand,
                dealerHand: Hand,
                deck: Deck,
                timeOut: Duration): Action ={

    val statisticalCreate = new BlackJackStatisticalCreater(userHand,deck,timeOut)

    try{

      val statistical = statisticalCreate.create()

      val probabilityOfBlackJack = new BlackJackProbs(userHand.sum, deck, statistical)

      val strategy = new DetailsStrategy(probabilityOfBlackJack)
      println("詳細結果")
      strategy.nextAction()
    }
    catch {
      case _: Exception =>
        val strategy = new BasicStrategy(userHand,dealerHand)
        println("簡略結果")
        strategy.nextAction()
    }

  }
}
