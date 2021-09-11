package com.analysis.blackJack.app

import com.analysis.common.*
import com.analysis.common.util.RichIterable.*
import com.analysis.blackJack.util.DeckUtili.*
import com.analysis.blackJack.util.HandUtil.*
import com.analysis.blackJack.util.ActionUtil.*
import com.analysis.blackJack.util.SystemCommandUtil.*
import com.analysis.blackJack.*
import com.analysis.common.io.CommandReader.*
import com.analysis.blackJack.strategy.{BasicStrategy, DetailsStrategy}
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode



/*
オブジェクト名     Analyze_BlackJack
機能             プレイ中のブラックジャックにおいて最善手を計算
説明             blackJack¥package.scalaにてエイリアス、アクション、コマンドを定義
*/
object AnalyzeBlackJack {
  import com.analysis.blackJack.io.AnalyzeBlackJackIO._

  def main(args: Array[String]): Unit = {
    println("処理を開始します。")

    val optDeck = init()

    if (optDeck.isEmpty) return
    val deck = optDeck.get

    mainFlow(deck)

    println("処理を終了します。")
  }

  /*
  メソッド名    init
  機能         システム(山札)の初期化
              mainFlowの実行
  */
  def init(): Option[Deck] = {
    val deckNum = readDeckNumber(5)

    for {
      n <- deckNum
    } yield createDeck(n)
  }

  /*
  メソッド名     mainFlow
  機能          プレイ中のブラックジャックにおいて最善手を計算
  引数          deck: Deck    使用中の山札
  */
  def mainFlow(deck: Deck): Unit = {
    import com.analysis.blackJack.app._

    val optPairResultOfUserHitA = readHand("[手札入力] : ユーザの手札を一枚ずつ入力してください。", deck)
    if (optPairResultOfUserHitA.isEmpty) return
    val pairResultOfUserHitA = optPairResultOfUserHitA.get

    val optPairResultOfUserHitB =
      readHand("[手札入力] : ユーザの手札を一枚ずつ入力してください。", pairResultOfUserHitA.deck)
    if (optPairResultOfUserHitB.isEmpty) return
    val pairResultOfUserHitB = optPairResultOfUserHitB.get

    val optPairResultOfDealerHitA =
      readHand("[手札入力] : ディーラの手札を入力してください。", pairResultOfUserHitB.deck)
    if (optPairResultOfDealerHitA.isEmpty) return
    val pairResultOfDealerHitA = optPairResultOfDealerHitA.get

    val userHand    : Hand = pairResultOfUserHitA.hand ++ pairResultOfUserHitB.hand
    val dealerHand  : Hand = pairResultOfDealerHitA.hand
    val deckReadHand: Deck = pairResultOfDealerHitA.deck

    val optUserAction: Option[Action] = CalculationFlow.flow(userHand,dealerHand,deckReadHand)
    if (optUserAction.isEmpty) return
    val userAction = optUserAction.get

    val optDeckAftAction: Option[Deck] = ActionFlow.flow(userHand,dealerHand,deckReadHand,userAction)
    if (optDeckAftAction.isEmpty) return
    val deckAftAction = optDeckAftAction.get

    val optDealerHitHand = readDealerHitHand()
    if (optDealerHitHand.isEmpty) return
    val dealerHitHand = optDealerHitHand.get

    val dealerHandCompleted = dealerHand ++ dealerHitHand
    println(s"ディーラの手札は ${dealerHandCompleted.mkString("[", ",", "]")} でした。")

    val deckCompleted = deckAftAction diff dealerHitHand

    val optSystemCommand = readSystemCommand()

    optSystemCommand match {
      case Some(Continue) => mainFlow(deckCompleted)
      case Some(Init) =>
        val optNewDeck = init()
        if (optNewDeck.isEmpty) return
        val newDeck = optNewDeck.get
        mainFlow(newDeck)
      case _ => return
    }
  }
}
