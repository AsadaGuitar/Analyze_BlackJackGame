package com.analysis.blackJack.util

/*
オブジェクト名　　　　DeckUtil
機能             　山札に関するユーティリティオブジェクト
*/
object DeckUtili {

  //エイリアス定義
  type Deck = Vector[Int]

  /*
  メソッド名     createDeck
  機能          山札の生成
  引数          number: Int       山札の個数
  戻値          生成した山札
  */
  def createDeck(number: Int): Deck ={
    //使用するトランプの種類を定義
    val trumpSet = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10)

    //山札を個数分追加
    def add(num: Int, deck: Deck): Deck = {
      if (num <= 1) deck
      else deck ++ add(num - 1, deck)
    }
    //個数が0の場合空の山札を返却
    if (number < 1) Vector.empty
    else add(number * 4, trumpSet)
  }
}
