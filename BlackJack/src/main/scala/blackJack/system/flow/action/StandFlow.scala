package blackjack.system.flow.action

import blackjack.{Deck, SystemCommand}
import blackjack.system.parts.BlackJackIO.{readDealerHitHand, readSystemCommand}

class StandFlow(deck: Deck) extends ActionFlow(deck) {

  override def action(): (SystemCommand,Deck) ={

    val (hands, deckDeletedDealerHands) = readDealerHitHand(deck)
    println("入力値")
    hands.foreach(println)

    val systemCommand = readSystemCommand()
    (systemCommand, deckDeletedDealerHands)
  }
}
