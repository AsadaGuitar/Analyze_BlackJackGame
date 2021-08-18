package blackJack

import scala.annotation.tailrec

/*
オブジェクト名   BlackJackIO
機能           プレイヤーからの入力値を取得
 */
object BlackJackIO {

  /*
  クラス名    Player
  機能       プレイヤーの切替を担う
   */
  abstract class Player
  case object User extends Player
  case object UserHit extends Player
  case object Dealer extends Player

  /*
  メソッド名   readDeckNum
  機能        山札の個数を取得
  返値        Int     入力値
   */
  @tailrec
  def readDeckNum(): Int = {
    println("使用する山札の数(1~10)を入力してください。")
    val n = io.StdIn.readLine()
    if ((1 to 10).map(_.toString) contains n) n.toInt
    else {
      println("入力値が不正です。")
      readDeckNum()
    }
  }

  /*
  メソッド名   readHand
  機能        手札の取得
  返値        Int       入力値
   */
  @tailrec
  def readHand(player: Player): Int = {
    println(
      player match {
        case User =>  "ユーザの手札を1枚ずつ入力してください。"
        case UserHit => "ヒットした手札を入力してください。"
        case Dealer => "ディーラの手札を入力してください。"
      }
    )
    val n = io.StdIn.readLine()
    if ((1 to 10).map(_.toString) contains n) n.toInt
    else {
      println("入力値が不正です。")
      readHand(player)
    }
  }

  /*
  メソッド名   readDealerHands
  機能        手札を複数取得
  返値        Hands       入力値
   */
  @tailrec
  def readDealerHands(hands: Hands): Hands ={
    println("ディーラの手札を入力してください。\n終了する場合は\"q\"を入力してください。")
    val n = io.StdIn.readLine()
    if ((1 to 10).map(_.toString) contains n) readDealerHands(hands :+ n.toInt)
    else if(n.equals("q") && hands.nonEmpty) hands
    else {
      println("入力値が不正です。")
      readDealerHands(hands)
    }
  }

  /*
  メソッド名   readAction
  機能        アクションを取得
  返値        Action         入力値
   */
  @tailrec
  def readAction(): Action ={
    val actions = Seq(("HIT",Hit),("STAND",Stand),("DOUBLE-DOWN",DoubleDown))
    println("\"Hit\",\"Stand\",\"Double-Down\"のいずれかのアクションを入力してください。")
    val inputAction = io.StdIn.readLine()
    if(actions.map(_._1).contains(inputAction.toUpperCase)) {
      val optAction = actions.find(_._1==inputAction.toUpperCase)
      optAction.get._2
    }
    else {
      println("入力値が不正です。")
      readAction()
    }
  }

  /*
  メソッド名   readGameCommand
  機能        コマンドを取得
  返値        GameCommand         入力値
   */
  @tailrec
  def readGameCommand(): GameCommand ={
    val gameCommands = Seq(("CONTINUE",Continue),("INIT-DECK",InitDeck),("FINISH",Finish))
    println("\"Continue\",\"Init-Deck\",\"Finish\"のいずれかのコマンドを入力してください。")
    val inputCommand = io.StdIn.readLine()
    if(gameCommands.map(_._1).contains(inputCommand.toUpperCase)) {
      gameCommands.find(_._1==inputCommand.toUpperCase).map(_._2).get
    }
    else {
      println("入力値が不正です。")
      readGameCommand()
    }
  }
}