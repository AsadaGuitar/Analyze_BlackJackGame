package blackjack.calculation.statistical.probability.strategy

import blackjack._
import blackjack.calculation.statistical.probability.ProbabilityOfBlackJack

class DetailsStrategy(val probs: ProbabilityOfBlackJack) extends Strategy {

  override def nextAction(): Action = {

    val result: Option[Action] = for{
      userBurst <- probs.userBurstProb
      dealerLose <- probs.dealerLoseProb
      dealerWin <- probs.dealerWinProb
    } yield {
      if (userBurst < 0.1) Hit
      else if (dealerLose > 0.7) Stand
      else if (dealerWin > 0.8) Hit
      else Stand
    }

    result.get
  }
}
