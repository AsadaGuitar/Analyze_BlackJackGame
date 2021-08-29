package blackJack.calculation.probabilityStatistics

import blackJack.*
import blackJack.calculation.Rational

import collection.parallel.CollectionConverters.ImmutableSeqIsParallelizable
import scala.annotation.tailrec
import scala.collection.parallel.immutable.*
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions

object HandStatisticsCreater {

  implicit class RichDeck(deck: Deck) {
    def :-(tramp: Tramp): Deck = deck.diff(Seq(tramp))
  }

  def exchangeAce(hand: Hand): Hand ={
    @tailrec
    def affect(hand: Hand, counter: Int)(fn: Hand => Hand): Hand =
      if (counter == 0) hand
      else affect(fn(hand), counter -1)(fn)

    @tailrec
    def exchange(hand: Hand, counter: Int): Hand ={
      if (counter == 0) return hand
      val deletedAce = affect(hand,counter)(h => h diff Seq(1))
      val addedEleven = affect(deletedAce,counter)(h => h :+ 11)

      if ((17 to 21).contains(addedEleven.sum)) addedEleven
      else exchange(hand, counter -1)
    }
    exchange(hand, hand.count(_==1))
  }
}

trait HandStatisticsCreater {
  import HandStatisticsCreater._

  def loopHit(hand: Hand, deck: Deck): Seq[Hand] = hand match{
      case h if 17 <= h.sum => Seq(h)
      case h if 17 <= exchangeAce(h).sum => Seq(exchangeAce(h))
      case _ => for {
        tramp <- deck
        result <- loopHit(hand :+ tramp, deck :- tramp)
      } yield result
    }

  private val futureLoopHit: (Hand,Deck) => Future[Seq[Hand]] =
    (hand: Hand, deck: Deck) =>
      Future(
        if (17 <= exchangeAce(hand).sum) Seq(exchangeAce(hand))
        else loopHit(hand,deck))

  def paralellLoopHit(hand: Hand, deck: Deck): Future[Seq[Seq[Hand]]] = {
    val paralell: ParSeq[Future[Seq[Hand]]] = deck.toSeq.par.map(x => futureLoopHit(hand :+ x, deck :- x))
    Future.sequence(paralell.seq)
  }
}

//並列 7 => 6.619s
//delay 5.663s
//lDelay 26.553s
//並列非同期 7 => 6.296s
//delay 13.068s
//lDelay 21.638s
//非同期 7 => 6.804s
//delay 54.503s
//lDelay 23.467s
