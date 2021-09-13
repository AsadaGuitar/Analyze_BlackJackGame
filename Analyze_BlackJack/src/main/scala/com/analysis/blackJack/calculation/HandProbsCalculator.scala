package com.analysis.blackJack.calculation

import com.analysis.blackJack.util.HandUtil._
import com.analysis.common.calculation.Probs._
import com.analysis.common.calculation._
import com.analysis.common.calculation.Rational
import com.analysis.blackJack.util.DeckUtili._
import javax.swing.DefaultCellEditor
import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, FiniteDuration}

/*
オブジェクト名     HandProbsCalculator
機能　　　　　     現在の手札から引く可能性のある手札とその確率を計算
*/
object HandProbsCalculator {

  /*
  メソッド名     calculateProb
  機能　　　     山札と手札を受取り、手札の長さからその手札の確率を計算
  引数　　　     hand: Hand      手札
  　　　　　     deck: Deck      山札
  戻値　　　     その手札を引く確率
  */
  private def calculateProb(hand: Hand,deck: Deck): Rational = {
    val range: List[Int] = (((deck.length - hand.length) + 2) to deck.length).toList
    val denom = range.reduce((acc,x) => acc * x)
    new Rational(1, BigInt(denom))
  }
  
  /*
  メソッド名     loopHit
  機能　　　     スコアが17以上になるまで山札からトランプを引いた手札を返却
  引数　　　     hand: Hand      手札
  　　          deck: Deck      山札
  戻値　　　     Seq[Hand]       初めに受け取った手札から引く可能性のある手札のリストを返却
  */
  private def loopHit(hand: Hand, deck: Deck): Seq[Hand] = hand match{
      case h if 17 <= h.sum             => Seq(h)
      case h if 17 <= h.exchangeAce.sum => Seq(h.exchangeAce)
      case _ => deck.flatMap(t => loopHit(hand :+ t, deck.diff(Seq(t))))
  }

  /*
  メソッド名     futureLoopHit
  機能　　　     loopHitの並列実行
  引数　　　     hand: Hand          手札
  　　　　　     deck: Deck          山札
  戻値          Future[Seq[Hand]]   非同期実行で実行するfutureLoopHit
  */
  private val futureLoopHit: (Hand,Deck) => Future[Seq[Hand]] =
    (hand: Hand, deck: Deck) => Future {
      if (17 <= hand.exchangeAce.sum) Seq(hand.exchangeAce) 
      else loopHit(hand, deck)
    }

  /*
  メソッド名     calculate
  機能　　　     ディーラの手札を受取り、ディーラが引く可能性のある手札のスコアと確率を保持するProbsクラスを返却
  引数　　　     dealerHand       : Hand         ディーラの手札
  　　　　　     deck             : Deck         山札
  　　　　　     implicit  timeout: Duration     タイムアウトを要求する時間
  戻値　　　     Either[conurrent.TimeoutException,Probs[Int]]   ディーラが引く可能性のある手札のスコアと確率を保持するProbsクラス
  */
  def calculate(dealerHand: Hand, deck: Deck) 
               (implicit timeout: Duration): Either[concurrent.TimeoutException, Probs[Int]]= {
    
    //計算を非同期で並列実行
    val paralellCalculate: Future[Vector[Seq[Hand]]] = 
      Future.sequence {
        for {
          trump <- deck
        } yield futureLoopHit(dealerHand :+ trump, deck.diff(Seq(trump)))
      }
    
    //計算がタイムアウトした場合Leftを返却、時間以内に完了した場合結果を返却
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
