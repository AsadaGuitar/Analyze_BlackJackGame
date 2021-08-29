package blackJack.calculation.strategy

import scala.language.postfixOps
import scala.language.implicitConversions

import blackJack.*
import blackJack.calculation.Rational
import blackJack.calculation.probabilityStatistics.ProbabilityStatistics
import blackJack.calculation.strategy.*

object DetailsStrategy {

  def accuracyRate(implicit probs: ProbabilityStatistics[Int]) = probs.get(_ => true)

  def dealerBurstRate(implicit probs: ProbabilityStatistics[Int]) = probs.get(21 < _)

  def dealerWinRate(userScore: Int)(implicit probs: ProbabilityStatistics[Int]) =
    probs.get(x => x <= 21 && userScore < x)

  def dealerLoseRate(userScore: Int)(implicit probs: ProbabilityStatistics[Int]) =
    for {
      burst <- probs.get(21 < _)
      under <- probs.get(_ < userScore)
    } yield burst + under

  def userBurstRate(userScore: Int, deck: Deck)(implicit probs: ProbabilityStatistics[Int]) = {
    val count: Int = deck.count(x => 21 < userScore + x)
    Some(new Rational(count, deck.length))
  }
  def userWinRate(userScore: Int)(implicit probs: ProbabilityStatistics[Int]) =
    for {
      burst <- probs.get(21 < _)
      under <- probs.get(_ < userScore)
    } yield burst + under

  def userLoseRate(userScore: Int)(implicit probs: ProbabilityStatistics[Int]) =probs.get(x => x <= 21 && userScore < x)

  def drawRate(userScore: Int)(implicit probs: ProbabilityStatistics[Int]) = probs.get(_ == userScore)

}

class DetailsStrategy(probabilityStatistics: ProbabilityStatistics[Int]) {
  import DetailsStrategy._

  private implicit def probs: ProbabilityStatistics[Int] = probabilityStatistics

  private implicit def getRate(rational: Option[Rational]): BigDecimal = rational match {
    case Some(x) => x.get()
    case _ => 0
  }

  def bestAction(user: Hand, deck: Deck): Action =
    if (dealerBurstRate < 0.1) Hit
    else if (dealerLoseRate(user.sum) > 0.7) Stand
    else if (dealerWinRate(user.sum) > 0.8) Hit
    else Stand
}
