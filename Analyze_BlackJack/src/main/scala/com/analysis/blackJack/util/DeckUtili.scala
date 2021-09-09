package com.analysis.blackJack.util

object DeckUtili {

  type Deck = Vector[Int]

  def createDeck(number: Int): Deck ={
    val trumpSet = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10)

    def add(num: Int, deck: Deck): Deck = {
      if (num <= 1) deck
      else deck ++ add(num - 1, deck)
    }
    //個数が0の場合空の山札を返却
    if (number < 1) Vector.empty
    else add(number * 4, trumpSet)
  }
}
