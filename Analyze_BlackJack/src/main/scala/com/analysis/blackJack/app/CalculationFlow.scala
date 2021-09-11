package com.analysis.blackJack.app

import com.analysis.blackJack.io.AnalyzeBlackJackIO.readAction
import com.analysis.blackJack.calculation.BestActionCalculator
import com.analysis.blackJack.util.ActionUtil.Action
import com.analysis.blackJack.util.DeckUtili.Deck
import com.analysis.blackJack.util.HandUtil.Hand

import scala.language.postfixOps
import concurrent.duration.DurationInt
import concurrent.duration.Duration

object CalculationFlow {

  def flow(userHand: Hand, dealerHand: Hand, deck: Deck): Option[Action] = {
    println("計算を開始します。")

    implicit val timeout: Duration = 10 second
    val bestAction = BestActionCalculator.calculate(userHand,dealerHand,deck)
    println(s"最善手は${bestAction}です。")

    readAction()
  }
}