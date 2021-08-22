package blackjack.calculation.statistical.probability

import blackjack._
import blackjack.calculation.statistical.ProbsStatistical


class BlackJackProbs (userScore: Int,
                      deck: Deck,
                      statistical: ProbsStatistical[Int])
  extends Probs[Int](statistical.toMap) with ProbabilityOfBlackJack {

  override def accuracyProb = map ?= (_ => true)

  override def dealerBurstProb = map ?= (21 < _)

  override def dealerWinProb = map ?= (x => x <= 21 && userScore < x)

  override def dealerLoseProb =
    for {
      burst <- map ?= (21 < _)
      under <- map ?= (_ < userScore)
    } yield burst + under


  override def userBurstProb = {
    val count: Int = deck.count(x => 21 < userScore + x)
    Some(new Rational(count, deck.length))
  }

  override def userWinProb =
    for {
      burst <- map ?= (21 < _)
      under <- map ?= (_ < userScore)
    } yield burst + under

  override def userLoseProb = map ?= (x => x <= 21 && userScore < x)

  override def drawProb = map ?= (_ == userScore)
}