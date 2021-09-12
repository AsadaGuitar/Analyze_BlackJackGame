package com.analysis.blackJack.io

import com.analysis.blackJack.util.ActionUtil.{Action, toAction}
import com.analysis.blackJack.util.DeckUtili.Deck
import com.analysis.blackJack.util.HandUtil.{Hand, toHand}
import com.analysis.blackJack.util.SystemCommandUtil.{SystemCommand, toSystemCommand}

/*
オブジェクト名      AnalyzeBlackJackIO
機能              AnalyzeBlackJackで使用するIOプログラムのユーティリティオブジェクト   
*/
object AnalyzeBlackJackIO {

  import com.analysis.common.io.CommandReader.*

  /*
  メソッド名     readDeckNumber
  機能          山札の個数を読込
  引数          max:  Int       読込む数値の最大値
  戻値          Option[Int]     読込んだ数値
  */
  def readDeckNumber(max: Int): Option[Int] = {
    println(s"[山札初期化] : 山札の個数、1 ~ ${max} を入力してください。")
    
    readCommand()((str: String) => str match {
      //1から引数指定の数値が入力された場合入力値を返却
      case s if ((1 to max).map(_.toString).contains(s)) => Right(s.toInt)
      //指定外の数値を読込んだ場合Exception引数の文字列を出力
      case _ => Left(new IllegalArgumentException("入力値が不正です。"))
    })
  }

  /*
  クラス名      PairResultOfHit
  機能         readHand関数の戻値       
  メンバ       hand:  Hand        山札から引いたトランプ
  　　　       deck:  Deck        ヒット後の山札
  */
  case class PairResultOfHit(hand: Hand, deck: Deck)

  /*
  メソッド名     readHand
  機能          手札の読込
  引数          msg:  String              出力するメッセージ
  　　          deck: Deck                使用中の山札
  戻値          Option[PairResultOfHit]   山札から引いたトランプとヒット後の山札を保有したクラス
  */
  def readHand(msg: String, deck: Deck): Option[PairResultOfHit] = {
    //メッセージ出力
    println(msg)
    //手札読込
    val hitHand = readCommand()(toHand)
    hitHand.map(h => PairResultOfHit(h, deck diff h))
  }

  /*
  メソッド名     readDealerHitHand
  機能          ディーラがヒットしたトランプを取得
  戻値          Option[Hand]        ディーラがヒットしたトランプを手札として返却
  */
  def readDealerHitHand(): Option[Hand] = {
    println(
      s"""|ディーラがHitした手札を一枚ずつ入力してください。
          |終了する場合は\"q\"を入力してください。""".stripMargin)
    val handList = readCommandList(readingPeriod = "q")(toHand)
    handList.map(x => x.flatten)
  }

  /*
  メソッド名       readAction
  機能            アクションの読込
  戻値            Option[Action]      読込んだアクション
  */
  def readAction(): Option[Action] = {
    println("[アクション入力] : \"Hit\",\"DoubleDown\",\"Stand\"のいずれかを入力してください。")
    readCommand()(toAction)
  }

  /*
  メソッド名       readSystemCommand
  機能            システムコマンドの読込
  戻値            Option[SystemCommand]      読込んだシステムコマンド
  */
  def readSystemCommand(): Option[SystemCommand] = {
    println("\"Continue\",\"Init\",\"Finish\"のいずれかを入力してください。")
    readCommand()(toSystemCommand)
  }
}
