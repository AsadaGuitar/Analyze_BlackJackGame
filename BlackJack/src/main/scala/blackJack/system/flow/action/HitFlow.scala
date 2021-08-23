package blackjack.system.flow.action

import blackjack.system.flow.calculation._
import blackjack.system.parts.BlackJackIO.{readBlackJackHand, readDealerHitHand, readSystemCommand}
import blackjack.{Deck, Hand, SystemCommand}

import scala.concurrent.duration.Duration

class HitFlow(userHand: Hand, dealerHand: Hand, deck: Deck)
             (implicit val timeout: Duration)
  extends ActionFlow(deck) {

  override def action(): (SystemCommand,Deck) = {

    val (burst,hitHand,deckDeletedUserHand) = isBurstHitHand(userHand,deck)

    if(burst) {
      print("バーストしましたので、")
      val (_,deckDealerDeletedDeck) = readDealerHitHand(deckDeletedUserHand)
      (readSystemCommand(),deckDealerDeletedDeck)
    }
    else{
      val calculator = new CalculationFlowAbstract(hitHand,dealerHand,deck)
      val newAction = calculator.startCalculation()
      ActionFlow.create(newAction,hitHand,dealerHand,deckDeletedUserHand).action()
    }
  }

  def isBurst(hand: Hand): Boolean = 21 < hand.sum

  def isBurstHitHand(userHand: Hand, deck: Deck): (Boolean,Hand,Deck) ={

    val (hitTramp, deckDeletedUserHand) = readBlackJackHand("ユーザー", deck)
    println(s"入力値 : ${hitTramp.tramps.head}")

    if (isBurst(userHand + hitTramp)) (true, userHand + hitTramp, deckDeletedUserHand)
    else (false, userHand + hitTramp, deckDeletedUserHand)
  }
}
