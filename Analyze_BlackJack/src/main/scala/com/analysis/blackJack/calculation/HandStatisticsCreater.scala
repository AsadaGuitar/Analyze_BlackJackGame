package com.analysis.blackJack.calculation

import com.analysis.blackJack._
import com.analysis.common._

import collection.parallel.CollectionConverters.ImmutableSeqIsParallelizable
import scala.annotation.tailrec
import scala.collection.parallel.immutable.*
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions


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
