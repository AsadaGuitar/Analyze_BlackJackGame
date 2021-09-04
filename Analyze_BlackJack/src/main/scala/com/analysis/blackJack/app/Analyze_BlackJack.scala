package com.analysis.blackJack.app

import com.analysis.common.*
import com.analysis.common.io.MonadicIO.*
import com.analysis.common.util.RichDiffDefine._
import com.analysis.blackJack.*
import com.analysis.blackJack.io.BlackJackIO.*
import com.analysis.blackJack.calculation.HandProbsCalculatorImpl
import com.analysis.blackJack.strategy.{BasicStrategy, DetailsStrategy}
import com.analysis.blackJack.util.DeckUtility._
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.language.postfixOps


/*
オブジェクト名     Analyze_BlackJack
機能             プレイ中のブラックジャックにおいて最善手を計算
説明             blackJack¥package.scalaにてエイリアス、アクション、コマンドを定義
*/
object Analyze_BlackJack extends IOApp {

  private val deckMsg    = "[山札初期化] : 山札の個数、1 ~ 5 を入力してください。"
  private val deckErrMsg = "入力値が不正です。"
  private val deckMaxNum = 5

  private val userHandMsg   = "[手札入力] : ユーザの手札を一枚ずつ入力してください。"
  private val dealerHandMsg = "[手札入力] : ディーラの手札を入力してください。"
  private val handErrMsg    = "[入力値不正] : 1 ~ 10 の数値を入力してください。"

  private val actionMsg    = "[アクション入力] : \"Hit\",\"DoubleDown\",\"Stand\"のいずれかを入力してください。"
  private val actionErrMsg = "<<入力値不正>>"

  private val dealerHitHandPeriod = "q"
  private val dealerHitHandMsg    = s"""|ディーラがHitした手札を一枚ずつ入力してください。
                                        |終了する場合は\"${dealerHitHandPeriod}\"を入力してください。""".stripMargin
  private val dealerHitHandErrMsg = "[入力値不正] : 1 ~ 10 の数値を入力してください。"

  private val systemCommandMsg    = "\"Continue\",\"Init\",\"Finish\"のいずれかを入力してください。"
  private val systemCommandErrMsg = "<<入力値不正>>"


  override def run(args: List[String]) = {
    (putStrLn("Welcome to Analyze_BlackJack") *> init()).as(ExitCode.Success)
  }

  /*
  メソッド名    init
  機能         システム(山札)の初期化
              mainFlowの実行
  */
  def init(): IO[Unit] = {
    putStrLn(deckMsg) *> readInt().flatMap(x => x match {
      case Right(n) => 
        if ((1 to deckMaxNum).contains(n)){

          val deck = createDeck(n)

          if (deck.nonEmpty) mainFlow(deck)
          else putStrLn(deckErrMsg) *> init()
        }
        else {
          putStrLn(deckErrMsg) *> init()
        }
      case Left(_) => putStrLn(deckErrMsg) *> init()
    })
  }

  /*
  メソッド名     mainFlow
  機能          プレイ中のブラックジャックにおいて最善手を計算
  引数          deck: Deck    使用中の山札
  */
  def mainFlow(deck: Deck): IO[Unit] = for {

    userHandA   <- readTrumpRepetition(userHandMsg, handErrMsg)
    userHandB   <- readTrumpRepetition(userHandMsg, handErrMsg)
    userHand = userHandA ++ userHandB
    dealerHand  <-  readTrumpRepetition(dealerHandMsg, handErrMsg)
    deckFirstDeleted = deck.diff(userHand ++ dealerHand)

    bestAction = calculateFlow(userHand, dealerHand, deckFirstDeleted)
    _               <- putStrLn(s"最善手は${bestAction}です。")
    userAction      <- readActionRepetition(actionMsg,actionErrMsg)
    deckAfterAction <- actionFlow(userHand,dealerHand,deckFirstDeleted,userAction)

    dealerHitHand <- readDealerHitTrumps(dealerHitHandMsg, dealerHitHandErrMsg, dealerHitHandPeriod)
    dealerHandCompleted = dealerHand ++ dealerHitHand
    _             <- putStrLn(s"ディーラの手札は ${dealerHandCompleted.mkString("[", ",", "]")} でした。")
    deckCompleted = deckAfterAction diff dealerHitHand

    userSystemCommand <- readSystemCommandRepetition(systemCommandMsg, systemCommandErrMsg)

    _ <- userSystemCommand match {
      case Continue => mainFlow(deckCompleted)
      case Init     => init()
      case Finish   => putStrLn("終了します。")
    }
  } yield ()


  /*
  メソッド名     calculate
  機能          ディーラのスコアの確率統計を作成、分析し、最善手を返却
  引数          user: Hand        ユーザの手札
  　　          dealer: Hand      ディーラの手札
  　　          deck: Deck        使用中の山札
  戻値          Action            確率統計に基づいて算出された最善手
  */
  def calculateFlow(user: Hand, dealer: Hand, deck: Deck): Action = {
    println("計算を開始します。")

    val handProbsCreater = new HandProbsCalculatorImpl(dealer, deck)
    val probs = handProbsCreater.calculate(10 second)

    probs match {
      case Right(x) =>
        val detailsStrategy = new DetailsStrategy(x)
        detailsStrategy.bestAction(user, deck)
      case Left(e) =>
        println("タイムアウトしました")
        BasicStrategy.bestAction(user, dealer)
    }
  }

  /*
  メソッド名     actionFlow
  機能          入力されたアクションの処理を実行
  引数          user: Hand        ユーザの手札
  　　          dealer: Hand      ディーラの手札
  　　          deck: Deck        使用中の山札
  　　          action: Action    入力されたアクション
  戻値          Deck              アクション後の山札
  */
  def actionFlow(user: Hand, dealer: Hand, deck: Deck, action: Action): IO[Deck] = {

    action match {
      case Hit => for {

        hitHand <- readTrumpRepetition(userHandMsg, handErrMsg)
        afterHitHand = user ++ hitHand
        deckDeleted  = deck diff afterHitHand

        bestAction   = calculateFlow(afterHitHand, dealer, deckDeleted)
        _ <- putStrLn(s"最善手は${bestAction}です。")

        userActoin <- readActionRepetition(actionMsg,actionErrMsg)
        deckCompletedActionFlow <- actionFlow(afterHitHand, dealer, deckDeleted, userActoin)

      } yield deckCompletedActionFlow

      case Stand => IO(deck)

      case DoubleDown =>
        val hitHand = readTrumpRepetition(userHandMsg, handErrMsg)
        hitHand.map(n => deck diff n)
    }
  }
}
