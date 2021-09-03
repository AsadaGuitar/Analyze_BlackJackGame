package com.analysis.blackJack.calculation

import com.analysis.blackJack.*
import com.analysis.common.calculation.{ProbabilityStatistics, ProbsCreater, Rational}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

import com.analysis.common.calculation._

 /*
クラス名      HandProbabilityStatisticsCreater
機能         手札の確率統計を生成
引数         dealerHand: Hand       ディーラの手札
            usingDeck: Deck         使用中の山札
*/
class HandProbsCreater(dealerHand: Hand, usingDeck: Deck) 
   extends HandProbabilityCalculator
     with HandStatisticsCreater {
   
   /*
   メソッド名   create
   機能        手札の確率統計を生成
   引数        implicit timeout: Duration                                          タイムアウトの設定
   戻値        Either[concurrent.TimeoutException,ProbabilityStatistics[Int]]      生成した確率統計
   */
   override def create(implicit timeout: Duration): Either[concurrent.TimeoutException, Probs[Int]]={
     
     try {
       //統計を並列、非同期処理で取得
       val asynchronousResult: Future[Seq[Seq[Hand]]] = paralellLoopHit(dealerHand,usingDeck)
       
       //非同期の待機時間が設定されたタイムアウトの時間を超過した場合、例外が発生
       val statistics = Await.result(asynchronousResult, timeout)
       
       //統計の確率を集計
       val probsList = for {
         hand <- statistics.flatten
       } yield (hand.sum, calculateProb(hand,usingDeck))
       
       //確率統計を生成
       Right(probsList.toProbs)
     }
     catch {
       //確率統計生成時、例外が発生した場合はEither.Leftを返却
       case e : concurrent.TimeoutException => Left(e)
     }
   }
}

