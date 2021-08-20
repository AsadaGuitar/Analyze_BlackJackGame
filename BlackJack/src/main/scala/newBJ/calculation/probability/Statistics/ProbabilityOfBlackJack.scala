package newBJ.calculation.probability.Statistics

import newBJ.Deck
import newBJ.calculation.probability.Rational


trait ProbabilityOfBlackJack {

  def accuracyProb: Option[Rational]

  def dealerBurstProb: Option[Rational]

  def dealerWinProb(userScore: Int): Option[Rational]

  def dealerLoseProb(userScore: Int): Option[Rational]

  def userBurstProb(userScore: Int, deck: Deck): Option[Rational]

  def userWinProb(userScore: Int): Option[Rational]

  def drawProb(userScore: Int): Option[Rational]
}
