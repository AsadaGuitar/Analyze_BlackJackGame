package com.analysis.blackJack.calculation

import com.analysis.blackJack.calculation.*
import com.analysis.blackJack.strategy.{BasicStrategy, DetailsStrategy}
import com.analysis.blackJack.util.ActionUtil.*
import com.analysis.blackJack.util.HandUtil.*
import com.analysis.blackJack.util.DeckUtili.*

import scala.concurrent.duration.Duration

object BestActionCalculator {
  
  def calculate(userHand: Hand, dealerHand: Hand, deck: Deck)
               (implicit timeout: Duration): Action ={
    val probs = HandProbsCalculator.calculate(dealerHand,deck)
    probs match {
      case Right(x) => DetailsStrategy.bestAction(userHand,deck)(x)
      case Left(e)  => BasicStrategy.bestAction(userHand, dealerHand)
    }
  }
  
}



