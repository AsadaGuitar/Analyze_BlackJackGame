package blackJack.calculation.probabilityStatistics

import blackJack.*
import blackJack.calculation.Rational

import collection.parallel.CollectionConverters.VectorIsParallelizable
import scala.collection.parallel.immutable._

import scala.language.implicitConversions

object HandStatisticsCreater {

  implicit class RichDeck(deck: Deck) {
    def :-(tramp: Tramp) = deck diff Seq(tramp)
  }

  implicit def fixDimensionsVector[T](ll: Vector[Vector[T]]): Vector[T] =
    for {l <- ll; e <- l} yield e

  implicit def parVectorToSeq[T](par: ParVector[T]): Vector[T] = par.seq
}

trait HandStatisticsCreater {
  import HandStatisticsCreater._
  
  def paralellLoopHit(hand: Hand, deck: Deck): Vector[Hand] ={

    val exchangedHand = hand.exchangeAce((17 to 21).contains)

    if (17 <= hand.sum) {
      Vector(hand)
    }
    else if (17 <= exchangedHand.sum) {
      Vector(exchangedHand)
    }
    else
      val parDeck: ParVector[Tramp] = deck.par
      for{
        tramp <- parDeck
        l <- paralellLoopHit(hand + tramp, deck :- tramp)
      } yield l
  }
}
