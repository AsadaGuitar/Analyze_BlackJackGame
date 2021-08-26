package blackJack.calculation.strategy

import blackJack.Deck
import blackJack.calculation.Rational
import blackJack.calculation.probabilityStatistics.ProbabilityStatistics

trait RateComputable(probabilityStatistics: ProbabilityStatistics[Int]) {
  
  private val _probs = probabilityStatistics
  protected def probs = _probs
  
  def accuracyRate = probs ?= (_ => true)
  
  def dealerBurstRate = probs ?= (21 < _)

  def dealerWinRate(userScore: Int) = probs ?= (x => x <= 21 && userScore < x)
  
  def dealerLoseRate(userScore: Int) =
    for {
      burst <- probs ?= (21 < _)
      under <- probs ?= (_ < userScore)
    } yield burst + under
  
  def userBurstRate(userScore: Int, deck: Deck) = {
    val count: Int = deck.count(x => 21 < userScore + x)
    Some(new Rational(count, deck.length))
  }

  def userWinRate(userScore: Int) =
    for {
      burst <- probs ?= (21 < _)
      under <- probs ?= (_ < userScore)
    } yield burst + under

  def userLoseRate(userScore: Int) = probs ?= (x => x <= 21 && userScore < x)

  def drawRate(userScore: Int) = probs ?= (_ == userScore)
}
