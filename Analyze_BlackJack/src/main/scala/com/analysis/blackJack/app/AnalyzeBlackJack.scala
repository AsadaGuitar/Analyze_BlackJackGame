package com.analysis.blackJack.app

import com.analysis.common.*
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

    //山札を生成
    val optDeck = init()

    //山札が存在する場合、処理を開始
    if (optDeck.isDefined) {
      val deck = optDeck.get
      mainFlow(deck)
    }

    println("処理を終了します。")
  }

  /*
  メソッド名    init
  機能         システム(山札)の初期化
              mainFlowの実行
  */
  def init(): Option[Deck] = {
    
    //山札の個数を取得
    val deckNum = readDeckNumber(5)

    //山札の生成
    for {
      n <- deckNum
    } yield createDeck(n)
  }

  /*
  メソッド名     mainFlow
  機能          プレイ中のブラックジャックにおいて最善手を計算
  引数          deck: Deck    使用する山札
  */
  def mainFlow(deck: Deck): Unit = {
    import com.analysis.blackJack.app._

    //ユーザ手札読込
    val optPairResultOfUserHitA = readHand("[手札入力] : ユーザの手札を一枚ずつ入力してください。", deck)
    //存在しない場合、処理終了
    if (optPairResultOfUserHitA.isEmpty) return
    val pairResultOfUserHitA = optPairResultOfUserHitA.get

    //ユーザ手札読込
    val optPairResultOfUserHitB =
      readHand("[手札入力] : ユーザの手札を一枚ずつ入力してください。", pairResultOfUserHitA.deck)
    //存在しない場合、処理終了
    if (optPairResultOfUserHitB.isEmpty) return
    val pairResultOfUserHitB = optPairResultOfUserHitB.get

    //ディーラ手札読込
    val optPairResultOfDealerHitA =
      readHand("[手札入力] : ディーラの手札を入力してください。", pairResultOfUserHitB.deck)
    //存在しない場合、処理終了
    if (optPairResultOfDealerHitA.isEmpty) return
    val pairResultOfDealerHitA = optPairResultOfDealerHitA.get

    //入力値の定義
    val userHand    : Hand = pairResultOfUserHitA.hand ++ pairResultOfUserHitB.hand
    val dealerHand  : Hand = pairResultOfDealerHitA.hand
    val deckReadHand: Deck = pairResultOfDealerHitA.deck

    //計算フローを実行、及びユーザアクションの読込
    val optUserAction: Option[Action] = CalculationFlow.flow(userHand,dealerHand,deckReadHand)
    //存在しない場合、処理終了
    if (optUserAction.isEmpty) return
    val userAction = optUserAction.get

    //アクションフローを実行、及びフロー後の山札を取得
    val optDeckAftAction: Option[Deck] = ActionFlow.flow(userHand,dealerHand,deckReadHand,userAction)
    //存在しない場合、処理終了
    if (optDeckAftAction.isEmpty) return
    val deckAftAction = optDeckAftAction.get

    //ディーラがヒットしたトランプの読込
    val optDealerHitHand = readDealerHitHand()
    //存在しない場合、処理終了
    if (optDealerHitHand.isEmpty) return
    val dealerHitHand = optDealerHitHand.get

    //ディーラがヒットした後の手札を標準出力
    val dealerHandCompleted = dealerHand ++ dealerHitHand
    println(s"ディーラの手札は ${dealerHandCompleted.mkString("[", ",", "]")} でした。")

    //処理終了後の山札を定義
    val deckCompleted = deckAftAction diff dealerHitHand

    //システムコマンド読込
    val optSystemCommand = readSystemCommand()
    
    optSystemCommand match {
      //Continueを選択した場合、再度実行
      case Some(Continue) => mainFlow(deckCompleted)
      //Initを選択した場合、山札を初期化して再度実行
      case Some(Init) =>
        val optNewDeck = init()
        if (optNewDeck.isEmpty) return
        val newDeck = optNewDeck.get
        mainFlow(newDeck)
      //存在しない場合、処理終了
      case _ => return
    }
  }
}
