package com.analysis.blackJack.app

import com.analysis.blackJack.util.ActionUtil.{Action, DoubleDown, Hit, Stand}
import com.analysis.blackJack.util.DeckUtili.Deck
import com.analysis.blackJack.util.HandUtil.Hand

/*
オブジェクト名     ActionFlow
機能　　　　　     メイン関数のユーザアクションのフローを保持
*/
object ActionFlow {

  import com.analysis.blackJack.io.AnalyzeBlackJackIO._

  /*
  メソッド名     hitFlow
  機能　　　     メイン関数のヒットアクションのフロー
  引数　　　     uHand : Hand      ユーザ手札
  　　　　　     dHand : Hand      ディーラ手札
  　　　　　     deck  : Deck      山札
  　　　　　     action: Action    ユーザが選択したアクション
  戻値　　　     Option[Deck]      ユーザアクション後の山札
  */
  private def hitFlow(uHand: Hand, dHand: Hand, deck: Deck, action: Action) : Option[Deck] = action match {
    //ユーザがヒットを選択した場合
    case Hit =>
      
      //ユーザの手札を標準入力で取得
      val optPairResultOfUserHit = readHand("[手札入力] : ユーザの手札を入力してください。", deck)
      //ユーザがシステムを終了する文字列を入力した場合
      if (optPairResultOfUserHit.isEmpty) None
      val pairResultOfUserHit = optPairResultOfUserHit.get

      //ヒット後の手札を定義
      val handAftHit  = uHand ++ pairResultOfUserHit.hand
      //ヒット後の山札を定義
      val deckReadHit = pairResultOfUserHit.deck

      //ユーザのアクションを標準入力で取得
      val optNextAction = CalculationFlow.flow(handAftHit,dHand,deckReadHit)
      //ユーザがシステムを終了する文字列を入力した場合
      if (optNextAction.isEmpty) None
      val nextAction = optNextAction.get

      //再度実行
      hitFlow(handAftHit, dHand, deckReadHit, nextAction)
      
    //ユーザがヒット以外を選択した場合
    case _ => Some(deck)
  }

  /*
  メソッド名     flow
  機能　　　     メイン関数のユーザアクションのフロー
  引数　　　     userHand    : Hand      ユーザ手札
  　　　　　     dealerHand  : Hand      ディーラ手札
  　　　　　     deck        : Deck      山札
  　　　　　     action      : Action    アクション
  戻値　　　     Option[Deck]            ユーザアクション後の山札
  */
  def flow (userHand: Hand, dealerHand: Hand, deck: Deck, action: Action): Option[Deck] = action match {
    case Hit        => hitFlow(userHand, dealerHand, deck, action)
    case Stand      => Some(deck)
    case DoubleDown => readHand("[手札入力] : ユーザの手札を一枚ずつ入力してください。", deck).map(_.deck)
  }
}
