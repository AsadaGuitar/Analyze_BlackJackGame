package com.analysis.blackJack.app

import com.analysis.common._
import com.analysis.common.io.MonadicIO._
import com.analysis.common.util.RichSequence
import com.analysis.blackJack._
import com.analysis.blackJack.io.BlackJackIO._
import com.analysis.blackJack.calculation.HandProbsCreater
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
object Analyze_BlackJack extends IOApp{

  def readHandRepetition(msg: String, errMsg: String, filter: String => Boolean): IO[Int] ={
    putStrLn(msg) *>
      readInt(user).flatMap(x => x match {
        case Right(x) if (filter(x)) => IO(x)
        case Left(_)                 => putStrLn(errMsg) *> hitHand()
      })
  }

  def readActionRepetition(msg: String, errMsg: String): IO[Action] = {
    putStrLn(msg) *>
      readAction().flatMap(x => x match {
        case Right(a) => IO(a)
        case Left(_)  => putStrLn(mrrMsg) *> readActionRepetition()
      })
  }

  def readSystemCommandRepetition(msg: String, errMsg: String): IO[SystemCommand] = {
    putStrLn(msg) *>
      readSystemCommand().flatMap(x => x match {
        case Right(a) => IO(a)
        case Left(_)  => putStrLn(mrrMsg) *> readActionRepetition()
      })
  }

  override def run(args: List[String]) = {
    (putStrLn("Welcome to Analyze_BlackJack") *> init()).as(ExitCode.Success)
  }

  /*
  メソッド名    init
  機能         システム(山札)の初期化
              mainFlowの実行
  */
  def init(): IO[Unit] = {

    val deckMsg    = "[山札初期化] : 山札の個数、1 ~ 5 を入力してください。"
    val deckErrMsg = "入力値が不正です。"
    val handFilter: String => Boolean = (x: String) => (1 to 5).map(_.toString).contains(x)

    val inputNum: IO[Either[NumberFormatException,Int]] = for{
      printMsg <- putStrLn(deckMsg)
      num      <- readInt()
    } yield num

    inputNum.flatMap(x => x match {
      case Right(n) => 
        if (handFilter(n)){
          val deck = createDeck(n)
          if (deck.nonEmpty) mainFlow(deck.get)
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
  def mainFlow(deck: Deck): IO[Unit] = {

    val userHandMsg   = "[手札入力] : ユーザの手札を一枚ずつ入力してください。"
    val dealerHandMsg = "[手札入力] : ディーラの手札を入力してください。"
    val handErrMsg    = "[入力値不正] : 1 ~ 10 の数値を入力してください。"
    val handFilter: String => Boolean = (x: String) => (1 to 10).map(_.toString).contains(x)

    val userHand: IO[Hand] = for {
      x <- readHandRepetition(userHandMsg, handErrMsg, handFilter)
      y <- readHandRepetition(userHandMsg, handErrMsg, handFilter)
    } yield Seq(x,y)

    val dealerHand: IO[Hand] = readHandRepetition(dealerHandMsg, handErrMsg, handFilter)

    val deckFirstDeleted: IO[Hand] = for {
      user   <- userHand
      dealer <- dealerHand
    } yield deck diff (user :+ dealer)

    def printBestHand() = for{
      user   <- userHand
      dealer <- dealerHand
      deck   <- deckFirstDeleted
      bestHand = calculateFlow(user, dealer, deck)
      _ <- putStrLn(s"最善手は${bestHand}です。")
    } yield ()


    val actionMsg    = "[アクション入力] : \"Hit\",\"DoubleDown\",\"Stand\"のいずれかを入力してください。"
    val actionErrMsg = "<<入力値不正>>"

    val userAction = readActionRepetition(actionMsg,actionErrMsg)

    val deckCompletedAction = {
      userHand.flatMap(user =>
        dealerHand.flatMap(dealer =>
          deckFirstDeleted.flatMap(deck =>
            userAction.flatMap(action => actionFlow(user, dealer, deck, action)))))
    }


    val dealerHitHandPeriod = "q"
    val dealerHitHandMsg    = s"""|ディーラがHitした手札を一枚ずつ入力してください。
                                  |終了する場合は\"${dealerHitHandPeriod}\"を入力してください。"""
    val dealerHitHandErrMsg = "[入力値不正] : 1 ~ 10 の数値を入力してください。"

    def readDealerHitHand(msg: String, errMsg: String, period: String): IO[Hand] = {
      def function(hand: Hand): IO[Hand] = {
        putStrLn(msg) *>
          //文字列読込
          readLn().flatMap(line => line match {
            //読込を終了する文字列を受取った場合、リストを返却
            case ln if (ln == period) => IO(tramps)
            //指定の文字列を受取った場合、リストに追加し再度実行
            case ln if (filter(ln)) => function(hand :+ ln.toInt)
            //不正な文字列を受取った場合、例外メッセージを出力し再度実行
            case _ => putStrLn(errMsg) *> function(hand)
          })
      }
      function(Nil)
    }

    val dealerHitHand: IO[Hand] =
      readDealerHitHand(dealerHitHandMsg, dealerHitHandErrMsg, dealerHitHandPeriod)

    val dealerHandCompleted: IO[Hand] = for {
      x <- dealerHand
      y <- dealerHitHand
    } yield x ++ y

    val deckCompletedDealerHit = for {
      hand <- dealerHandCompleted
      deck <- deckCompletedAction
    } yield deck diff hand

    def printDealerHand(): IO[Unit] = for {
      hand  <- dealerHandCompleted
      print <- putStrLn(s"ディーラの手札は ${hand.mkString("[", ",", "]")} でした。")
    } yield ()


    val systemCommandMsg    = "\"Continue\",\"Init\",\"Finish\"のいずれかを入力してください。"
    val systemCommandErrMsg = "<<入力値不正>>"

    val systemCommand = readSystemCommandRepetition(systemCommandMsg, systemCommandErrMsg)

    systemCommand.flatMap(cmd => cmd match {
      case Continue => mainFlow(completedDeck)
      case Init     => init()
      case Finish   => IO(println("終了します。"))
    })
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

    val handMsg    = "[手札入力] : ユーザの手札を一枚ずつ入力してください。"
    val handErrMsg = "[入力値不正] : 1 ~ 10 の数値を入力してください。"
    val handFilter: String => Boolean = (x: String) => (1 to 10).map(_.toString).contains(x)

    action match {
      case Hit =>

        val actionMsg    = "[アクション入力] : \"Hit\",\"DoubleDown\",\"Stand\"のいずれかを入力してください。"
        val actionErrMsg = "[入力値不正]"

        val hitHand: IO[Int] = readHandRepetition(handMsg,handErrMsg,handFilter)
        val afterHitHand = hitHand.map(n => user :+ n)
        val deckDeleted = hitHand.map(n => deck :- n)

        val bestAction: IO[Action] = for {
          user <- afterHitHand
          deck <- deckDeleted
        } yield calculate(user,dealer,deck)

        afterHitHand.flatMap(user =>
          deckDeleted.flatMap(deck =>
            readActionRepetition(actionMsg, actionErrMsg).flatMap(action =>
              putStrLn(s"【分析完了】最善手は${action}です。") *>
              actionFlow(user,dealer,deck,action))))

      case Stand =>
        IO(deck)

      case DoubleDown =>
        readHandRepetition(handMsg,handErrMsg,handFilter)
          .map(n => deck :- n)
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
  def calculateFlow(user: Hand, dealer: Hand, deck: Deck): Action = {
    println("計算を開始します。")

    val handProbsCreater = new HandProbsCreater(dealer, deck)
    val probs = handProbsCreater.create(10 second)

    probs match {
      case Right(x) =>
        val detailsStrategy = new DetailsStrategy(x)
        detailsStrategy.bestAction(user, deck)
      case Left(e) =>
        println("タイムアウトしました")
        BasicStrategy.bestAction(user, dealer)
    }
  }
}
