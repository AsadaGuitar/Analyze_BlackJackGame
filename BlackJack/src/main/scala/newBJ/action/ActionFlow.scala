package newBJ.action

import newBJ.{Deck, Hand, SystemCommand}

object ActionFlow {

  sealed abstract class Action
  case object Hit extends Action
  case object DoubleDown extends Action
  case object Stand extends Action

//  def create(action: Action): ActionFlow = action match {
//    case Hit => new HitAction()
//    case DoubleDown => new DoubleDownAction()
////    case Stand => new StandAction()
//  }
}

abstract class ActionFlow {

  def systemCommand: SystemCommand
  def action(userHand: Hand, dealerHand: Hand, deck: Deck): Deck
}