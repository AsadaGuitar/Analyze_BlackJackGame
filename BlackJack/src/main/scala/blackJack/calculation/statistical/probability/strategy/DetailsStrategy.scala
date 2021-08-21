package blackjack.calculation.statistical.probability.strategy

import blackjack._
import blackjack.calculation.statistical.probability.ProbabilityOfBlackJack

class DetailsStrategy(val probs: ProbabilityOfBlackJack) extends Strategy {

  override def nextAction(): Action = probs match {
    case p if 0.1 > p.userBurstProb => Hit
    case p if 0.7 < p.dealerLoseProb => Stand
    case p if 0.8 < p.dealerWinProb => Hit
  }
}
