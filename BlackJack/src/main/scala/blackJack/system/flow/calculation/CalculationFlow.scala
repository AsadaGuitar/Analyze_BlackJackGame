package blackjack.system.flow.calculation

import blackjack.calculation.statistical.probability.BlackJackProbs
import blackjack.calculation.statistical.probability.strategy.{BasicStrategy, DetailsStrategy}
import blackjack.system.parts.BlackJackIO.readAction
import blackjack.system.parts.{Computable, ProbsPrintable}
import blackjack.{Action, Deck, Hand}

import scala.concurrent.duration.Duration
import scala.language.postfixOps


abstract class CalculationFlow(val userHand: Hand,
                               val dealerHand: Hand,
                               val deck: Deck)

class CalculationFlowAbstract (userHand: Hand,
                               dealerHand: Hand,
                               deck: Deck)
                              (implicit val timeout: Duration)
  extends CalculationFlow(userHand,dealerHand,deck)
    with Computable
    with ProbsPrintable{

  require(deck.nonEmpty)

  def startCalculation(): Action ={

    val statistical = calculate(dealerHand, deck)

    val result = statistical match {
      case Some(s) =>

        println("詳細分析完了")

        val probs = new BlackJackProbs(userHand.sum,deck,s)
        printProbs(probs)

        val strategy = new DetailsStrategy(probs)
        strategy.nextAction()

      case None =>

        println("簡略分析完了")

        val strategy = new BasicStrategy(userHand,dealerHand)

        strategy.nextAction()
    }

    println(s"計算結果 : ${result}が最善手です。")

    readAction()
  }
}
