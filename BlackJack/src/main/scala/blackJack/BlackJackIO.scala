package blackJack

import scala.annotation.tailrec

object BlackJackIO {

  abstract class Player
  case object User extends Player
  case object Dealer extends Player

  @tailrec
  def readDeckNum(): Int = {
    println("使用する山札の数(1~10)を入力してください。")
    val n = io.StdIn.readLine()
    if ((1 to 10).map(_.toString) contains n) n.toInt
    else readDeckNum()
  }

  @tailrec
  def readHand(player: Player): Int = {
    println({
      if (player == User) "ユーザの手札を1枚ずつ教えてください。"
      else "ディーラの手札を教えてください。"
    })
    val n = io.StdIn.readLine()
    if ((1 to 11).map(_.toString) contains n) n.toInt
    else readHand(player)
  }

  @tailrec
  def readAction(): Action ={
    val actions = Seq(("HIT",Hit),("STAND",Stand),("DOUBLE-DOWN",DoubleDown))
    println("\"Hit\",\"Stand\",\"Double-Down\"のいずれかのアクションを入力してください。")
    val inputAction = io.StdIn.readLine()
    if(actions.map(_._1).contains(inputAction.toUpperCase)) {
      val optAction = actions.find(_._1==inputAction.toUpperCase)
      optAction.get._2
    }
    else readAction()
  }

  @tailrec
  def readGameCommand(): GameCommand ={
    val gameCommands = Seq(("START-GAME",StartGame),("CONTINUE",Continue),("INIT-DECK",InitDeck),("FINISH",Finish))
    println("\"Start-Game\",\"Continue\",\"Init-Deck\",\"Finish\"のいずれかのコマンドを入力してください。")
    val inputCommand = io.StdIn.readLine()
    if(gameCommands.map(_._1).contains(inputCommand.toUpperCase)) {
      gameCommands.find(_._1==inputCommand.toUpperCase).map(_._2).get
    }
    else readGameCommand()
  }
}
