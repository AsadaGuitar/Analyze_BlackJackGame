package newBJ.probability

import newBJ.Deck

object ProbabilityDetailsCalculator {

  def probabilityAccuracy(statistical: Statistical) = statistical.filterSum(_ => true)

  def dealerBurstProbability(statistical: Statistical) =
    statistical.filterSum(21 < _)

  def dealerWinProbability(statistical: Statistical, userScore: Int) =
    statistical.filterSum(x => x <= 21 && userScore < x)

  def dealerLoseProbability(statistical: Statistical, userScore: Int) =
    statistical.filterSum(x => 21 < x && x < userScore)

  def userBurstProbability(userScore: Int, deck: Deck) = {
    val count: Int = deck.count(x => 21 < userScore + x)
    new Rational(count, deck.length)
  }

  def userWinProbability(statistical: Statistical, userScore: Int) =
    statistical.filterSum(x => 21 < x && x < userScore)

  def drawProbability(statistical: Statistical, userScore: Int) =
    statistical.filterSum(_ == userScore)
}
