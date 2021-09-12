package com.analysis.blackJack.util


import com.analysis.blackJack._
import scala.annotation.tailrec

/*
オブジェクト名       HandUtil
機能               手札オブジェクトのユーティリティオブジェクト
*/
object HandUtil {

  //エイリアス定義
  type Hand = Seq[Int]

  //Handに変換する際のExceptionクラス
  final class HandFormatException(errMsg: String = "不正な文字列です") extends Exception(errMsg)

  /*
  メソッド名     toHand
  機能          StringオブジェクトをHandオブジェクトに変換
  引数          str: String      　　　　　　　　　　　　　　Handオブジェクトに変換するStringオブジェクト
  戻値          Either[HandFormatException, Hand] 　　　 Stringオブジェクトを変換したHandオブジェクト
  */
  def toHand(str: String): Either[HandFormatException, Hand] = str match {
    //1から10に含まれている場合Handオブジェクトを含有するEitherを返却
    case ln if ((1 to 10).map(_.toString).contains(ln)) => Right(Seq(ln.toInt))
    //1から10に含まれていない場合Exceptionを含有するEitherを返却
    case _ => Left(new HandFormatException())
  }

  /*
  暗黙クラス名        RichHand
  機能　　　　　　　　　Handオブジェクトにメソッドを追加
  引数　　　　　　　　　hand: Hand        対象のHandオブジェクト
  */
  implicit class RichHand(hand: Hand) {

    /*
    メソッド        isExistAce
    機能           Aceが含まれているかどうかの真偽値を返却
    戻値           Boolean      Aceが含まれているかどうかの真偽値
    */
    def isExistAce: Boolean = hand.exists(_==1)

    /*
    メソッド名       countAce
    機能            1がいくつ含まれている数を返却
    戻値            Int         1が含まれている数
    */
    def countAce: Int = hand.count(_==1)

    /*
    メソッド名　　　　　exchangeAce
    機能　　　　　　　　手札の1を11に変換
    戻値             Hand        1が11に変換されたHandオブジェクト
    */
    def exchangeAce: Hand ={

      //指定の回数Handオブジェクトを変換するメソッド
      @tailrec
      def affect(hand: Hand, counter: Int)(fn: Hand => Hand): Hand = {
        if (counter == 0) hand
        else affect(fn(hand), counter -1)(fn)
      }

      //手札から1を削除し、1の個数だけ11を加えるメソッド
      @tailrec
      def exchange(hand: Hand, counter: Int): Hand ={
        if (counter == 0) return hand
        //1を1要素分削除
        val deletedAce = affect(hand,counter)(h => h.diff(Seq(1)))
        //11を1要素分追加
        val addedEleven = affect(deletedAce,counter)(h => h :+ 11)

        //変換した手札が17～21に含まれている場合返却
        if ((17 to 21).contains(addedEleven.sum)) addedEleven
        else exchange(hand, counter -1)
      }
      exchange(hand, hand.count(_==1))
    }
  }
}
