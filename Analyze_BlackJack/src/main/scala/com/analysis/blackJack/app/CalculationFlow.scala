package com.analysis.blackJack.app

import com.analysis.blackJack.io.AnalyzeBlackJackIO.readAction
import com.analysis.blackJack.calculation.BestActionCalculator
import com.analysis.blackJack.util.ActionUtil.Action
import com.analysis.blackJack.util.DeckUtili.Deck
import com.analysis.blackJack.util.HandUtil.Hand

import scala.language.postfixOps
import concurrent.duration.DurationInt
import concurrent.duration.Duration

/*
オブジェクト名     CalculationFlow
機能　　　　　     メイン関数の計算フローを保持
*/
object CalculationFlow {

  /*
  メソッド      flow
  機能　　      メイン関数の計算フロー
  引数　　      userHand    :Hand     ユーザ手札
  　　　　      dealerHand  :Hand     ディーラ手札
  　　　　      deck        :Deck     山札
  戻値　　      Option[Action]        ユーザが入力したアクション
  */
  def flow(userHand: Hand, dealerHand: Hand, deck: Deck): Option[Action] = {
    println("計算を開始します。")

    implicit val timeout: Duration = 10 second
    
    //計算を実行、最善手を取得
    val bestAction = BestActionCalculator.calculate(userHand,dealerHand,deck)
    println(s"最善手は${bestAction}です。")

    //ユーザアクションの読込
    readAction()
  }
}