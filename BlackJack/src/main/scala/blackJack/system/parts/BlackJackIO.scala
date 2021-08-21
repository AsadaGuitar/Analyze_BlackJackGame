package blackjack.system.parts

import blackjack._

import scala.annotation.tailrec

object BlackJackIO {

  @tailrec
  private def read(): Hand ={
    val input = io.StdIn.readLine()

    try{
      val tramp = input.toInt
      new Hand(tramp)
    }
    catch{
      case _: Exception => read()
    }
  }

  def readHand(deck: Deck): (Hand,Deck) ={
    val hand = read()
    (hand, deck diff hand.tramps)
  }

  @tailrec
  def readDealerHitHand(hands: Seq[Hand], deck: Deck): (Seq[Hand],Deck) ={

    println("ディーラの手札を入力してください。\n終了する場合は\"q\"を入力してください。")
    val n = io.StdIn.readLine()
    if ((1 to 10).map(_.toString) contains n) {
      readDealerHitHand(
        hands :+ new Hand(n.toInt),
        deck diff Seq(n.toInt))
    }
    else if(n.equals("q") && hands.nonEmpty) (hands,deck)
    else {
      println("入力値が不正です。")
      readDealerHitHand(hands, deck)
    }
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
    else {
      println("入力値が不正です。")
      readAction()
    }
  }

  @tailrec
  def readSystemCommand(): SystemCommand ={

    val gameCommands = Seq(("CONTINUE",Continue),("INIT-DECK",InitDeck),("FINISH",Finish))
    println("\"Continue\",\"Init-Deck\",\"Finish\"のいずれかのコマンドを入力してください。")
    val inputCommand = io.StdIn.readLine()
    if(gameCommands.map(_._1).contains(inputCommand.toUpperCase)) {
      gameCommands.find(_._1==inputCommand.toUpperCase).map(_._2).get
    }
    else {
      println("入力値が不正です。")
      readSystemCommand()
    }
  }

}
