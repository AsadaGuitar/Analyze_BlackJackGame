package blackjack.calculation.statistical.probability

trait ProbabilityOfBlackJack {

  def accuracyProb: Option[Rational]

  def dealerBurstProb: Option[Rational]

  def dealerWinProb: Option[Rational]

  def dealerLoseProb: Option[Rational]

  def userBurstProb: Option[Rational]

  def userWinProb: Option[Rational]

  def drawProb: Option[Rational]
}
