package com.analysis.blackJack.app

import com.analysis.common._
import com.analysis.common.AnalysisIO._
import com.analysis.blackJack._
import com.analysis.blackJack.io.BlackJackIO._
import com.analysis.blackJack.calculation.HandProbsCreater
import com.analysis.blackJack.strategy.{BasicStrategy, DetailsStrategy}

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
object Analyze_BlackJack extends IOApp{


  override def run(args: List[String]) = {
    (putStrLn("Welcome to Analyze_BlackJack") *> init()).as(ExitCode.Success)
  }
  
  /*
  メソッド名   init
  機能        システム(山札)の初期化
              mainFlowの実行
  */
  def init(): IO[Unit] = {

    val deckMsg = "[山札初期化] : 山札の個数、1 ~ 5 を入力してください。"
    val deckErrMsg = "入力値が不正です。"
    val handFilter: String => Boolean = (x: String) => (1 to 5).map(_.toString).contains(x)

    val inputNum = for{
      msg <- putStrLn(deckMsg)
      num <- readInt()
    } yield num

    inputNum.flatMap(x => x match {
      case Right(n) => 
        if (handFilter(n)){
          val deck = createDeck(n)
          if (deck.nonEmpty) {
            mainFlow(deck.get)
          }
          else {
            putStrLn(deckErrMsg) *> init()
          }
        }
        else {
          putStrLn(deckErrMsg) *> init()
        }
      case _ => putStrLn(deckErrMsg) *> init()
    })
  }

  /*
  メソッド名     mainFlow
  機能          プレイ中のブラックジャックにおいて最善手を計算
  引数          deck: Deck    使用中の山札
  */
  def mainFlow(deck: Deck): IO[Unit] = {

    val userHandMsg = "[手札入力] : ユーザの手札を一枚ずつ入力してください。"
    val dealerHandMsg = "[手札入力] : ディーラの手札を入力してください。"
    val handFilter: String => Boolean = (x: String) => (1 to 10).map(_.toString).contains(x)
    val handErrMsg = "[入力値不正] : 1 ~ 10 の数値を入力してください。"
    
    for {

      userHandA <- readInt(userHandMsg, handErrMsg, handFilter)
      userHandB <- readInt(userHandMsg, handErrMsg, handFilter)

      dealerHand <- readInt(dealerHandMsg, handErrMsg, handFilter)

      bestHand = calculate(Seq(userHandA,userHandB), Seq(dealerHand), deck)

      _ <- putStrLn(s"最善手は${bestHand}です。")

      actionMsg = "[アクション入力] : \"Hit\",\"DoubleDown\",\"Stand\"のいずれかを入力してください。"
      actionErrMsg = "<<入力値不正>>"
      userAction <- readAction(actionMsg,actionErrMsg)

      actionDeck <- actionFlow(Seq(userHandA,userHandB), Seq(dealerHand), deck, userAction)

      dealerHitHandPeriod = "q"
      dealerHitHandMsg = s"ディーラがHitした手札を一枚ずつ入力してください。\n終了する場合は\"${dealerHitHandPeriod}\"を入力してください。"
      dealerHitHandErrMsg = handErrMsg

      dealerHitHand <- readHandRepetition(dealerHitHandMsg, dealerHitHandErrMsg, dealerHitHandPeriod, handFilter)
      dealerHandMsg = (dealerHand +: dealerHitHand).mkString("[", ",", "]")
      _ <- putStrLn(s"ディーラの手札は ${dealerHandMsg} でした。")

      completedDeck = actionDeck diff Seq(dealerHitHand)

      systemCommandMsg = "\"Continue\",\"Init\",\"Finish\"のいずれかを入力してください。"
      systemCommandErrMsg = "<<入力値不正>>"
      systemCommand <- readSystemCommand(systemCommandMsg, systemCommandErrMsg)

      nextFlow <- systemCommand match {
        case Continue => mainFlow(completedDeck)
        case Init => init()
        case Finish => IO(println("終了します。"))
      }
    } yield {
      nextFlow
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

    val userHandMsg = "[手札入力] : ユーザの手札を一枚ずつ入力してください。"
    val handErrMsg = "[入力値不正] : 1 ~ 10 の数値を入力してください。"
    val handFilter: String => Boolean = (x: String) => (1 to 10).map(_.toString).contains(x)

    val actionMsg = "[アクション入力] : \"Hit\",\"DoubleDown\",\"Stand\"のいずれかを入力してください。"
    val actionErrMsg = "[入力値不正]"

    action match {
      case Hit =>
        val result = for {
          //現在の手札を取得
          hitHand <- readInt(userHandMsg,handErrMsg,handFilter)
          afterHitHand = user :+ hitHand
          //ヒット分を削除した山札を取得
          deckDeleted = deck diff Seq(hitHand)
          //ディーラのスコアの確率統計を作成、分析し、最善手を取得
          bestAction = calculate(afterHitHand, dealer, deckDeleted)
          _ <- putStrLn(s"最善手は${bestAction}です。")
          userAction <- readAction(actionMsg,actionErrMsg)
        } yield {
          //アクションを実行
          (afterHitHand, dealer, deckDeleted, userAction)
        }
        result.flatMap(x => actionFlow(x._1, x._2, x._3, x._4))

      //山札をそのまま返却
      case Stand => IO(deck)

      case DoubleDown =>
        for{
          //ユーザがヒットした手札を取得
          hitHand <- readInt(userHandMsg, handErrMsg, handFilter)
        } yield {
          (deck diff user)
        }
    }
  }

  /*
  メソッド名     calculate
  機能          ディーラのスコアの確率統計を作成、分析し、最善手を返却
  引数          user: Hand        ユーザの手札
  　　          dealer: Hand      ディーラの手札
  　　          deck: Deck        使用中の山札
  戻値          Action            確率統計に基づいて算出された最善手
  */
  def calculate(user: Hand, dealer: Hand, deck: Deck): Action = {
    println("計算を開始します。")

    val probabilityStatisticsCreater = new HandProbsCreater(dealer, deck)
    val start = System.currentTimeMillis()
    val probabilityStatistics = probabilityStatisticsCreater.create(timeout = 1 second)
    val end = System.currentTimeMillis()
    println(s"${(end - start) / 1000.toDouble}s")

    probabilityStatistics match {
      case Right(x) =>
        println(x)
        val detailsStrategy = new DetailsStrategy(x)
        detailsStrategy.bestAction(user, deck)
      case Left(e) =>
        println("タイムアウトしました")
        BasicStrategy.bestAction(user, dealer)
    }
  }

  /*
  メソッド名   createDeck
  機能        ゲームで使用する山札の個数を受取り、山札を生成
  引数        deckNum: Int       山札の個数
  戻値        Option[Deck]    使用する山札
  */
  private def createDeck(deckNum: Int): Deck = {
    //トランプの組合わせを定義
    val deck: Deck = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10)

    //個数分、トランプの組合わせを追加
    def addTramps(num: Int, deck: Deck): Deck = {
      if (num <= 1) deck
      else deck ++ addTramps(num - 1, deck)
    }

    //個数が0の場合空の山札を返却
    if (deckNum < 1) Nil
    else addTramps(deckNum * 4, deck)
  }
}
