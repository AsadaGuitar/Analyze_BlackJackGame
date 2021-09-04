package com.analysis

import scala.annotation.tailrec

/*
メソッド名     blackJack
機能          blcakJackパッケージの静的クラス
*/
package object blackJack {

  //エイリアス定義
  type Trump = Int
  type Deck = Vector[Trump]
  type Hand = Seq[Trump]
  
  /*
  クラス名
  */ 
  implicit class RichHand(hand: Hand) {
    
    def countAce: Int = hand.count(_==1)
    
    def exchangeAce: Hand ={
      @tailrec
      def affect(hand: Hand, counter: Int)(fn: Hand => Hand): Hand =
        if (counter == 0) hand
        else affect(fn(hand), counter -1)(fn)

      @tailrec
      def exchange(hand: Hand, counter: Int): Hand ={
        if (counter == 0) return hand
        val deletedAce = affect(hand,counter)(h => h diff Seq(1))
        val addedEleven = affect(deletedAce,counter)(h => h :+ 11)

        if ((17 to 21).contains(addedEleven.sum)) addedEleven
        else exchange(hand, counter -1)
      }
      exchange(hand, hand.count(_==1))
    }
  }

  abstract class Action

  case object Hit extends Action

  case object Stand extends Action

  case object DoubleDown extends Action

  abstract class SystemCommand

  case object Continue extends SystemCommand

  case object Init extends SystemCommand

  case object Finish extends SystemCommand

}
