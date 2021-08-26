package blackJack.calculation.strategy

import scala.language.postfixOps
import blackJack.*
import blackJack.calculation.Rational
import blackJack.calculation.probabilityStatistics.ProbabilityStatistics
import blackJack.calculation.strategy.*

object DetailsStrategy {

  def getRate(rational: Option[Rational]): BigDecimal = rational match {
    case Some(x) => x.get()
    case _ => 0
  }
}

class DetailsStrategy(probs: ProbabilityStatistics[Int],
                      val user: Hand,
                      val deck: Deck) extends Strategy with RateComputable(probs) {
  import DetailsStrategy._

  override def nextAction(): Action = {

    if (getRate(dealerBurstRate) < 0.1) Hit
    else if (getRate(dealerLoseRate(user.sum)) > 0.7) Stand
    else if (getRate(dealerWinRate(user.sum)) > 0.8) Hit
    else Stand
  }
}
