package com.analysis.blackJack.calculation


import com.analysis.blackJack.util.HandUtil._
import com.analysis.common.calculation.*
import com.analysis.common.calculation.Probs
import com.analysis.common.util.RichIterable._
import com.analysis.blackJack.util.DeckUtili._
import javax.swing.DefaultCellEditor
import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, FiniteDuration}

import cats.Monad
import cats.FlatMap
import cats.Functor

trait HandProbsCalculator {
  
  def calculate(implicit timeout: Duration): Either[java.util.concurrent.TimeoutException, Probs[Int]]
}

class HandProbsCalculatorImpl(dealerHand: Hand, deck: Deck) extends HandProbsCalculator {

  private def calculateProb(hand: Hand,deck: Deck): Rational = {
    val r: List[Int] = (((deck.length - hand.length) + 2) to deck.length).toList
    new Rational(1, BigInt(productOfList(r)))
  }
  
  private def loopHit(hand: Hand, deck: Deck): Seq[Hand] = hand match{
      case h if 17 <= h.sum             => Seq(h)
      case h if 17 <= h.exchangeAce.sum => Seq(h.exchangeAce)
      case _ => deck.flatMap(t => loopHit(hand :+ t, deck :- t))
  }

  private val futureLoopHit: (Hand,Deck) => Future[Seq[Hand]] =
    (hand: Hand, deck: Deck) => Future {
      if (17 <= hand.exchangeAce.sum) Seq(hand.exchangeAce) 
      else loopHit(hand, deck)
    }

  override def calculate(implicit timeout: Duration): Either[concurrent.TimeoutException, Probs[Int]]= {

    val paralellCalculate = Future.sequence(deck.map(x => futureLoopHit(dealerHand :+ x, deck :- x)))

    val awaitResult: Either[concurrent.TimeoutException, Vector[Hand]] = try {
      //非同期の待機時間が設定されたタイムアウトの時間を超過した場合、例外が発生
      val statistics = Await.result(paralellCalculate, timeout)
      Right(statistics.flatten)
    }
    catch {
      case e: concurrent.TimeoutException =>
        Left(e)
    }
    //統計の確率を集計
    awaitResult.map(x => {
      for {
        hand <- x
      } yield (hand.sum, calculateProb(hand, deck))
    }.toProbs)
  }
}
