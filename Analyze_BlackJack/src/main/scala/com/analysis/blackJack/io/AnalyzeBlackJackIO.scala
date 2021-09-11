package com.analysis.blackJack.io

import com.analysis.blackJack.util.ActionUtil.{Action, toAction}
import com.analysis.blackJack.util.DeckUtili.Deck
import com.analysis.blackJack.util.HandUtil.{Hand, toHand}
import com.analysis.blackJack.util.SystemCommandUtil.{SystemCommand, toSystemCommand}

object AnalyzeBlackJackIO {

  import com.analysis.common.io.CommandReader.*

  def readDeckNumber(max: Int): Option[Int] = {
    println(s"[山札初期化] : 山札の個数、1 ~ ${max} を入力してください。")

    readCommand()((str: String) => str match {
      case s if ((1 to max).map(_.toString).contains(s)) => Right(s.toInt)
      case _ => Left(new IllegalArgumentException("入力値が不正です。"))
    })
  }

  case class PairResultOfHit(hand: Hand, deck: Deck)

  def readHand(msg: String, deck: Deck): Option[PairResultOfHit] = {
    println(msg)
    val hitHand = readCommand()(toHand)
    hitHand.map(h => PairResultOfHit(h, deck diff h))
  }

  def readDealerHitHand(): Option[Hand] = {
    println(
      s"""|ディーラがHitした手札を一枚ずつ入力してください。
          |終了する場合は\"q\"を入力してください。""".stripMargin)
    val handList = readCommandList(readingPeriod = "q")(toHand)
    handList.map(x => x.flatten)
  }

  def readAction(): Option[Action] = {
    println("[アクション入力] : \"Hit\",\"DoubleDown\",\"Stand\"のいずれかを入力してください。")
    readCommand()(toAction)
  }

  def readSystemCommand(): Option[SystemCommand] = {
    println("\"Continue\",\"Init\",\"Finish\"のいずれかを入力してください。")
    readCommand()(toSystemCommand)
  }
}
