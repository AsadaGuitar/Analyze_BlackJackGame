package com.analysis.blackJack.app

import com.analysis.blackJack.util.ActionUtil.{Action, DoubleDown, Hit, Stand}
import com.analysis.blackJack.util.DeckUtili.Deck
import com.analysis.blackJack.util.HandUtil.Hand

object ActionFlow {

  import com.analysis.blackJack.io.AnalyzeBlackJackIO._

  private def hitFlow(uHand: Hand, dHand: Hand, deck: Deck, action: Action) : Option[Deck] = action match {
    case Hit =>
      val optPairResultOfUserHit = readHand("[手札入力] : ユーザの手札を入力してください。", deck)
      if (optPairResultOfUserHit.isEmpty) None
      val pairResultOfUserHit = optPairResultOfUserHit.get

      val handAftHit  = uHand ++ pairResultOfUserHit.hand
      val deckReadHit = pairResultOfUserHit.deck

      val optNextAction = CalculationFlow.flow(handAftHit,dHand,deckReadHit)
      if (optNextAction.isEmpty) None
      val nextAction = optNextAction.get

      val deckCompleted = hitFlow(handAftHit, dHand, deckReadHit, nextAction)
      Some(deck)
    case _ => Some(deck)
  }

  def flow (userHand: Hand, dealerHand: Hand, deck: Deck, action: Action): Option[Deck] = action match {
    case Hit        => hitFlow(userHand, dealerHand, deck, action)
    case Stand      => Some(deck)
    case DoubleDown => readHand("[手札入力] : ユーザの手札を一枚ずつ入力してください。", deck).map(_.deck)
  }
}
