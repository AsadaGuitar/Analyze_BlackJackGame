package com.analysis.blackJack.app

import com.analysis.common.*
import com.analysis.common.util.RichIterable.*
import com.analysis.blackJack.util.DeckUtili.*
import com.analysis.blackJack.util.HandUtil.*
import com.analysis.blackJack.util.ActionUtil.*
import com.analysis.blackJack.util.SystemCommandUtil.*
import com.analysis.blackJack.*
import com.analysis.common.io.CommandReader.*
import com.analysis.blackJack.calculation.HandProbsCalculatorImpl
import com.analysis.blackJack.strategy.{BasicStrategy, DetailsStrategy}
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.language.{existentials, postfixOps}


/*
オブジェクト名     Analyze_BlackJack
機能             プレイ中のブラックジャックにおいて最善手を計算
説明             blackJack¥package.scalaにてエイリアス、アクション、コマンドを定義
*/
object Analyze_BlackJack {

  def main(args: Array[String]): Unit = {
    println("処理を開始します。")

    val deck = init()

    if (deck.isEmpty) return
    else mainFlow(deck.get)

    println("処理を終了します。")
  }
  
  /*
  メソッド名    init
  機能         システム(山札)の初期化
              mainFlowの実行
  */
  def init(): Option[Deck] = {

    val maxNum = 5
    println(s"[山札初期化] : 山札の個数、1 ~ ${maxNum} を入力してください。")

    val deckNum: Option[Int] = readCommand()((str: String) => str match {
      case s if ((1 to maxNum).map(_.toString).contains(s)) => Right(s.toInt)
      case _ => Left(new IllegalArgumentException("入力値が不正です。"))
    })

    deckNum.map(n => createDeck(n))
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
  
  def calculationFlow(userHand: Hand, dealerHand: Hand, deck: Deck) ={
    println("計算を開始します。")
    val bestAction = calculateBestAction(userHand, dealerHand, deck)
    println(s"最善手は${bestAction}です。")

    println("[アクション入力] : \"Hit\",\"DoubleDown\",\"Stand\"のいずれかを入力してください。")
    readCommand()(toAction)
  }

  case class HitPair(hand: Hand,deck: Deck)
  def hitHand(msg: String, deck: Deck): Option[HitPair] = {
    println(msg)
    val hitHand = readCommand()(toHand)
    hitHand.map(h => HitPair(h, deck diff h))
  }
  
  def ActionFlow(userHand: Hand, dealerHand: Hand, deck: Deck, userAction: Action): Option[Deck] = userAction match {
    case Hit =>
      def hitFlow(uHand: Hand, dHand: Hand, deck: Deck, action: Action): Option[Deck] = action match {
        case Hit => 
          
          val uHit = hitHand("[手札入力] : ユーザの手札を一枚ずつ入力してください。", deck)
          if (uHit.isEmpty) None
          
          val handAftHit  = uHand ++ uHit.get.hand
          val deckReadHit = uHit.get.deck
          
          val nextAction = calculationFlow(handAftHit, dHand, deckReadHit)
          if (nextAction.isEmpty) None
          
          val deckCompleted = hitFlow(handAftHit, dHand, deckReadHit, nextAction.get)
          Some(deck)

        case _ => Some(deck)
      }
      hitFlow(userHand, dealerHand, deck, userAction)

    case Stand => Some(deck)

    case DoubleDown => hitHand("[手札入力] : ユーザの手札を一枚ずつ入力してください。", deck).map(_.deck)
  }

  /*
  メソッド名     mainFlow
  機能          プレイ中のブラックジャックにおいて最善手を計算
  引数          deck: Deck    使用中の山札
  */
  def mainFlow(deck: Deck): Unit = {

    val uHitA = hitHand("[手札入力] : ユーザの手札を一枚ずつ入力してください。", deck)
    if (uHitA.isEmpty) return

    val uHitB = hitHand("[手札入力] : ユーザの手札を一枚ずつ入力してください。", uHitA.get.deck)
    if (uHitB.isEmpty) return

    val dHitA = hitHand("[手札入力] : ディーラの手札を入力してください。", uHitB.get.deck)
    if (dHitA.isEmpty) return

    val userHand    : Hand = uHitA.get.hand ++ uHitB.get.hand
    val dealerHand  : Hand = dHitA.get.hand
    val deckReadHand: Deck = dHitA.get.deck
    
    val userAction = calculationFlow(userHand, dealerHand, deckReadHand)
    if (userAction.isEmpty) return

    val deckAftAction: Option[Deck] = ActionFlow(userHand, dealerHand, deckReadHand, userAction.get)
    if (deckAftAction.isEmpty) return

    println(
      s"""|ディーラがHitした手札を一枚ずつ入力してください。
          |終了する場合は\"q\"を入力してください。""".stripMargin)

    val dHitHandList = readCommandList(readingPeriod = "q")(toHand)
    if (dHitHandList.isEmpty) return

    val dHandCompleted = dealerHand ++ dHitHandList.get.flatten
    println(s"ディーラの手札は ${dHandCompleted.mkString("[", ",", "]")} でした。")

    val deckCompleted = deckAftAction.get diff dHitHandList.get.flatten

    println("\"Continue\",\"Init\",\"Finish\"のいずれかを入力してください。")
    val uSystemCommand = readCommand()(toSystemCommand)

    uSystemCommand match {
      case Some(Continue) => mainFlow(deckCompleted)
      case Some(Init) =>
        val newDeck = init()
        if (newDeck.isEmpty) return
        else mainFlow(newDeck.get)
      case _ => return
    }
  }
}
