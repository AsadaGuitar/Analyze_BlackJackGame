package blackjack.system.flow.action

import blackjack.Deck
import blackjack.system.parts.BlackJackIO.{readBlackJackHand, readDealerHitHand, readSystemCommand}

class DoubleDownFlow(deck: Deck) extends ActionFlow(deck) {

  override def action() = {

    val (userHand, deckDeletedUserHand) = readBlackJackHand("ユーザー", deck)
    println(s"入力値 : $userHand")

    val (hands, deckDeletedDealerHands) = readDealerHitHand(deckDeletedUserHand)
    println("入力値")
    hands.foreach(println)

    val systemCommand = readSystemCommand()
    (systemCommand, deckDeletedDealerHands)
  }
}
