package blackjack.system.flow.action

import blackjack._

import scala.concurrent.duration.Duration

object ActionFlow {

  def create(action: Action,
             userHand: Hand,
             dealerHand: Hand,
             deck: Deck)(implicit timeout: Duration) = action match {

    case Hit => new HitFlow(userHand,dealerHand,deck)
    case Stand => new StandFlow(deck)
    case DoubleDown => new DoubleDownFlow(deck)
  }
}

abstract class ActionFlow (val deck: Deck){

  def action(): (SystemCommand,Deck)
}
