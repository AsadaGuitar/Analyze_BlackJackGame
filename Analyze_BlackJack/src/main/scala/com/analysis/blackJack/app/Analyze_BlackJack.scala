package com.analysis.blackJack.app

import com.analysis.common.*
import com.analysis.common.io.MonadicIO.*
import com.analysis.common.util.RichIterable._
import com.analysis.blackJack.util.HandUtil._
import com.analysis.blackJack.util.ActionUtil._
import com.analysis.blackJack.util.SystemCommandUtil._
import com.analysis.blackJack.*
import com.analysis.blackJack.io.BlackJackIO._
import com.analysis.blackJack.calculation.HandProbsCalculatorImpl
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
object Analyze_BlackJack extends IOApp {

  override def run(args: List[String]) = {
    (putStrLn("Welcome to Analyze_BlackJack") *> initDeck().flatMap(mainFlow)).as(ExitCode.Success)
  }

  /*
  メソッド名    init
  機能         システム(山札)の初期化
              mainFlowの実行
  */
  def initDeck(): IO[Deck] = {
    //トランプの組合わせを定義
    val deck: Deck = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10)

    val msg = "[山札初期化] : 山札の個数、1 ~ 5 を入力してください。"
    val errMsg = "入力値が不正です。"
    val maxNum = 5

    def createDeck(number: Int) = {
      def add(num: Int, deck: Deck): Deck = {
        if (num <= 1) deck
        else deck ++ add(num - 1, deck)
      }
      //個数が0の場合空の山札を返却
      if (number < 1) Vector()
      else add(number * 4, deck)
    }

    val toDeckNum: String => Either[NumberFormatException, Int] = 
      (str: String) => str match {
      case s if ((1 to maxNum).map(_.toString).contains(s)) => Right(s.toInt)
      case _ => Left(new NumberFormatException(errMsg))
    }

    val deckNum = readCommandRepetition(toDeckNum)
    putStrLn(msg) *> deckNum.map(createDeck)
  }


  def calculateBestAction(userHand: Hand, dealerHand: Hand, deck: Deck) ={
    implicit val timeout: Duration = 10 second

    val handProbsCreater = new HandProbsCalculatorImpl(dealerHand, deck)
    val probs = handProbsCreater.calculate(timeout)

    probs match {
      case Right(x) =>
        val detailsStrategy = new DetailsStrategy(x)
        detailsStrategy.bestAction(userHand, deck)
      case Left(e)  =>
        BasicStrategy.bestAction(userHand, dealerHand)
    }
  }

  /*
  メソッド名     mainFlow
  機能          プレイ中のブラックジャックにおいて最善手を計算
  引数          deck: Deck    使用中の山札
  */
  def mainFlow(deck: Deck): IO[Unit] = {

    val userHandMsg   = "[手札入力] : ユーザの手札を一枚ずつ入力してください。"
    val dealerHandMsg = "[手札入力] : ディーラの手札を入力してください。"
    val actionMsg    = "[アクション入力] : \"Hit\",\"DoubleDown\",\"Stand\"のいずれかを入力してください。"

    val dealerHitHandPeriod = "q"
    val dealerHitHandMsg    = s"""|ディーラがHitした手札を一枚ずつ入力してください。
                                          |終了する場合は\"${dealerHitHandPeriod}\"を入力してください。""".stripMargin
    val dealerHitHandErrMsg = "入力値が不正です。"

    val systemCommandMsg    = "\"Continue\",\"Init\",\"Finish\"のいずれかを入力してください。"

    for {
      _          <- putStrLn(userHandMsg)
      userHandA  <- readCommandRepetition(toHand)
      _          <- putStrLn(userHandMsg)
      userHandB  <- readCommandRepetition(toHand)
      userHand = userHandA ++ userHandB

      _          <- putStrLn(dealerHandMsg)
      dealerHand <- readCommandRepetition(toHand)
      deckFirstDeleted = deck.diff(userHand ++ dealerHand)

      _          <- putStrLn("計算を開始します。")
      bestAction = calculateBestAction(userHand, dealerHand, deckFirstDeleted)
      _          <- putStrLn(s"最善手は${bestAction}です。")

      _          <- putStrLn(actionMsg)
      userAction <- readCommandRepetition(toAction)

      deckAfterAction <- userAction match {
        case Hit =>
          def hitFlow(user: Hand, dealer: Hand, deck: Deck, action: Action): IO[Deck] = action match {
            case Hit => for {
              _       <- putStrLn(userHandMsg)
              hitHand <- readCommandRepetition(toHand)
              afterHitHand = user ++ hitHand
              deckDeleted  = deck.diff(afterHitHand)

              _ <- putStrLn("計算を開始します。")
              bestAction = calculateBestAction(afterHitHand, dealer, deckDeleted)
              _ <- putStrLn(s"最善手は${bestAction}です。")

              _                   <- putStrLn(actionMsg)
              nextAction          <- readCommandRepetition(toAction)
              deckCompletedAction <- hitFlow(afterHitHand, dealer, deckDeleted, nextAction)
            } yield deckCompletedAction
            case _ => IO(deck)
          }
          hitFlow(userHand, dealerHand, deckFirstDeleted, userAction)
          
        case Stand      => IO(deckFirstDeleted)
        
        case DoubleDown =>
          val hitHand = readCommandRepetition(toHand)
          putStrLn(userHandMsg) *> hitHand.map(n => deckFirstDeleted diff n)
      }

      _             <- putStrLn(dealerHitHandMsg)
      dealerHitHand <- readCommandUtilPeriod(dealerHitHandPeriod, toHand)
      dealerHandCompleted = dealerHand ++ dealerHitHand.flatten
      _             <- putStrLn(s"ディーラの手札は ${dealerHandCompleted.mkString("[", ",", "]")} でした。")
      deckCompleted = deckAfterAction diff dealerHitHand

      _                 <- putStrLn(systemCommandMsg)
      userSystemCommand <- readCommandRepetition(toSystemCommand)

      _ <- userSystemCommand match {
        case Continue => mainFlow(deckCompleted)
        case Init     => initDeck().flatMap(mainFlow)
        case Finish   => putStrLn("終了します。")
      }
    } yield ()
  } 
}
