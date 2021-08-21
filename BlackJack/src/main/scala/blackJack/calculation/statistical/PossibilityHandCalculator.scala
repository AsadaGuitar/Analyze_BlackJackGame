package blackjack.calculation.statistical

import blackjack._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait PossibilityHandCalculator {

  def loopHit(hand: Hand, deck: Deck, isInRange: Int => Boolean): Seq[Hand] ={

      val score: Int = hand.exchangeAce(isInRange).sum

      if (17 <= score) Seq(hand)
      else
        for{
          tramp <- deck
          l <- loopHit(hand + tramp, deck.diff(Seq(tramp)), isInRange)
        } yield l
    }

  val asynchronousLoopHit =
    (hand: Hand, deck: Deck, isInRange: Int => Boolean) =>
      Future(loopHit(hand,deck,isInRange))

  def parallelFindPossibility(hand: Hand, deck: Deck, isInRange: Int => Boolean) ={

    def parallelRun =
      for{tramp <- deck} yield asynchronousLoopHit(hand + tramp, deck diff Seq(tramp), isInRange)

    Future.sequence(parallelRun)
  }
}
