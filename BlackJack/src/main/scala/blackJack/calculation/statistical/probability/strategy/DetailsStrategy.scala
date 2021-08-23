package blackjack.calculation.statistical.probability.strategy

import blackjack._
import blackjack.calculation.statistical.probability.ProbabilityOfBlackJack

class DetailsStrategy(val probs: ProbabilityOfBlackJack) extends Strategy {

  override def nextAction(): Action = {

    probs.userBurstProb match {
      case Some(x) => if (x < 0.1) Hit
      case None =>
    }
    probs.dealerLoseProb match {
      case Some(x)  => if (x > 0.7) Stand
      case None =>
    }
    probs.dealerWinProb match {
      case Some(x) => if (x > 0.8) Hit
      case None =>
    }
    Stand
  }
}
