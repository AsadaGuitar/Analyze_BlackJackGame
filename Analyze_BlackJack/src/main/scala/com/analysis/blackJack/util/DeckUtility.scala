package com.analysis.blackJack.util

import com.analysis.blackJack.Deck
import com.analysis.blackJack._

object DeckUtility {

  /*
  メソッド名   createDeck
  機能        ゲームで使用する山札の個数を受取り、山札を生成
  引数        deckNum: Int       山札の個数
  戻値        Option[Deck]    使用する山札
  */
  def createDeck(deckNum: Int): Deck = {
    //トランプの組合わせを定義
    val deck: Deck = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10)

    //個数分、トランプの組合わせを追加
    def addTrumps(num: Int, deck: Deck): Deck = {
      if (num <= 1) deck
      else deck ++ addTrumps(num - 1, deck)
    }

    //個数が0の場合空の山札を返却
    if (deckNum < 1) Vector()
    else addTrumps(deckNum * 4, deck)
  }
}

