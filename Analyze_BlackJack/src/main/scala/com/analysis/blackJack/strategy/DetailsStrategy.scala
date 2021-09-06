package com.analysis.blackJack.strategy

import com.analysis.blackJack._
import com.analysis.blackJack.strategy.DetailsStrategy
import com.analysis.common.calculation.{Probs, Rational}
import com.analysis.blackJack.util.HandUtil._
import com.analysis.blackJack.util.ActionUtil._
import com.analysis.blackJack.util.SystemCommandUtil._

import scala.language.{implicitConversions, postfixOps}

object DetailsStrategy {

  def accuracyRate(implicit probs: Probs[Int]) = probs.get(_ => true)

  def dealerBurstRate(implicit probs: Probs[Int]) = probs.get(21 < _)

  def dealerWinRate(userScore: Int)(implicit probs: Probs[Int]) =
    probs.get(x => x <= 21 && userScore < x)

  def dealerLoseRate(userScore: Int)(implicit probs: Probs[Int]) =
    for {
      burst <- probs.get(21 < _)
      under <- probs.get(_ < userScore)
    } yield burst + under

  def userBurstRate(userScore: Int, deck: Deck)(implicit probs: Probs[Int]) = {
    val count: Int = deck.count(x => 21 < userScore + x)
    Some(new Rational(count, deck.length))
  }
  def userWinRate(userScore: Int)(implicit probs: Probs[Int]) =
    for {
      burst <- probs.get(21 < _)
      under <- probs.get(_ < userScore)
    } yield burst + under

  def userLoseRate(userScore: Int)(implicit probs: Probs[Int]) =probs.get(x => x <= 21 && userScore < x)

  def drawRate(userScore: Int)(implicit probs: Probs[Int]) = probs.get(_ == userScore)

}

class DetailsStrategy(probabilityStatistics: Probs[Int]) {
  import DetailsStrategy.*
  
  private implicit val probs: Probs[Int] = probabilityStatistics
  
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
