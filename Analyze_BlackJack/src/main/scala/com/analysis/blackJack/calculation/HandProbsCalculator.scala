package com.analysis.blackJack.calculation


import com.analysis.blackJack.{Deck,Hand,RichHand}
import com.analysis.common.calculation._
import com.analysis.common.calculation.Probs
import com.analysis.common.util.RichDiffDefine._
import com.analysis.common.util.ListUtil._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, FiniteDuration}


trait HandProbsCalculator {

  def calculate(implicit timeout: Duration): Either[java.util.concurrent.TimeoutException, Probs[Int]]
}

class HandProbsCalculatorImpl(dealerHand: Hand, deck: Deck) extends HandProbsCalculator {

  private def calculateProb(hand: Hand,deck: Deck): Rational = {
    val r: List[Int] = (((deck.length - hand.length) + 2) to deck.length).toList
    new Rational(1, BigInt(productOfList(r)))
  }
  
  private def loopHit(hand: Hand, deck: Deck): Seq[Hand] = hand match{
      case h if 17 <= h.sum => Seq(h)
      case h if 17 <= h.exchangeAce.sum => Seq(h.exchangeAce)
      case _ => for {
        trump <- deck
        result <- loopHit(hand :+ trump, deck :- trump)
      } yield result
  }

  private val futureLoopHit: (Hand,Deck) => Future[Seq[Hand]] =
    (hand: Hand, deck: Deck) =>
      Future(
        if (17 <= hand.exchangeAce.sum) Seq(hand.exchangeAce)
        else loopHit(hand, deck))


  private def paralellCalculate(hand: Hand, deck: Deck): Future[Vector[Seq[Hand]]] ={
    val paralellResult: Vector[Future[Seq[Hand]]] = deck.map(x => futureLoopHit(hand :+ x, deck :- x))
    Future.sequence(paralellResult)
  }

  override def calculate(implicit timeout: Duration): Either[concurrent.TimeoutException, Probs[Int]]= {

    //統計を並列、非同期処理で取得
    var asynchronousResult: Future[Vector[Seq[Hand]]] = paralellCalculate(dealerHand, deck)

    val awaitResult: Either[concurrent.TimeoutException, Vector[Hand]] = try {
      //非同期の待機時間が設定されたタイムアウトの時間を超過した場合、例外が発生
      val statistics = Await.result(asynchronousResult, timeout)
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

//並列 7 => 6.619s
//delay 5.663s
//lDelay 26.553s
//並列非同期 7 => 6.296s
//delay 13.068s
//lDelay 21.638s
//非同期 7 => 6.804s
//delay 54.503s
//lDelay 23.467s
