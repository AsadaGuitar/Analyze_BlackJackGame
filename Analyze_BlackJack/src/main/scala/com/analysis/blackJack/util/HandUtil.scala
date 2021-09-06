package com.analysis.blackJack.util

import blackJack.Hand
import com.analysis.common.util.RichIterable._

import scala.annotation.tailrec

object HandUtil {
  
  final class HandFormatException(errMsg: String = "不正な文字列です") extends Exception(errMsg)

  def toHand(str: String): Either[HandFormatException, Hand] = str match {
    case ln if ((1 to 10).map(_.toString).contains(ln)) => Right(Seq(ln.toInt))
    case _ => Left(new HandFormatException())
  }

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
        val deletedAce = affect(hand,counter)(h => h :- 1)
        val addedEleven = affect(deletedAce,counter)(h => h :+ 11)

        if ((17 to 21).contains(addedEleven.sum)) addedEleven
        else exchange(hand, counter -1)
      }
      exchange(hand, hand.count(_==1))
    }
  }
}
