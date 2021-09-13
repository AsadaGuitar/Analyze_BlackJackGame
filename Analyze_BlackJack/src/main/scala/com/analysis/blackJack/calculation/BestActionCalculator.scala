package com.analysis.blackJack.calculation

import com.analysis.blackJack.calculation.*
import com.analysis.blackJack.strategy.{BasicStrategy, DetailsStrategy}
import com.analysis.blackJack.util.ActionUtil.*
import com.analysis.blackJack.util.HandUtil.*
import com.analysis.blackJack.util.DeckUtili.*

import com.analysis.common.calculation._

import scala.concurrent.duration.Duration

/*
オブジェクト名     BestActionCalculator
機能　　　　　     ユーザ手札、ディーラ手札、山札からユーザの最善手を計算
*/
object BestActionCalculator {

  def printProbs(userScore: Int, deck: Deck)(implicit probs: Probs[Int]): Unit ={
    import DetailsStrategy._

    //確率をパーセント表記の文字列に変換
    def decorate(rate:  BigDecimal) = (rate.toFloat * 100).toString + "%"

    //確率の正確性を取得
    val accuracy   = getRate(accuracyRate)
    //ディーラの勝率を取得
    val dWinProb   = getRate(dealerWinRate(userScore))
    //ディーラの負率を取得
    val dLoseProb  = getRate(dealerLoseRate(userScore))
    //ディーラのバースト率を取得
    val dBurstProb = getRate(dealerBurstRate)
    //ユーザの勝率を取得
    val uWinProb   = getRate(userWinRate(userScore))
    //ユーザの負率を取得
    val uLoseProb  = getRate(userLoseRate(userScore))
    //ユーザのバースト率を取得
    val uBurstProb = getRate(userBurstRate(userScore,deck))
    //引き分けになる確率を取得
    val drawProb   = getRate(drawRate(userScore))

    println(
      s"""
      |確率の正確性      : ${decorate(accuracy)}
      |ディーラ勝率      : ${decorate(dWinProb)}
      |ディーラ負率      : ${decorate(dLoseProb)}
      |ディーラバースト率 : ${decorate(dBurstProb)}
      |ユーザ勝率　　　　 : ${decorate(uWinProb)}
      |ユーザ負率     　 : ${decorate(uLoseProb)}
      |ユーザバースト率　 : ${decorate(uBurstProb)}
      |ドロー率　　　　　 : ${decorate(drawProb)}
      |""".stripMargin)
  }

  /*
  メソッド名     calculate
  機能　　　     ユーザ手札、ディーラ手札、山札からユーザの最善手を計算
  引数　　　     userHand          :Hand       ユーザ手札
  　　　　　     dealerHand        :Hand       ディラー手札
  　　　　　     deck              :Deck       使用中の山札
  　　　　　     implicit  timeout :Duration   タイムアウトを要求する時間
  戻値　　　     Action                        計算結果のアクション
  */
  def calculate(userHand: Hand, dealerHand: Hand, deck: Deck)
               (implicit timeout: Duration): Action ={
    //確率統計を取得
    val probs = HandProbsCalculator.calculate(dealerHand,deck)
    probs match {
      //結果が返却された場合確率統計を使用し計算
      case Right(x) =>
        //確率を出力
        printProbs(userHand.sum,deck)(x)
        DetailsStrategy.bestAction(userHand,deck)(x)
      //タイムアウトした場合ベーシックストラテジーに基づき最善手を計算
      case Left(e)  => BasicStrategy.bestAction(userHand, dealerHand)
    }
  }
  
}



