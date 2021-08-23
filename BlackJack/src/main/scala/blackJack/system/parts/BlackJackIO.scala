package blackjack.system.parts

import blackjack._

import scala.annotation.tailrec

object BlackJackIO {

  @tailrec
  private def readInt(text: String): Int ={

    println(s"${text}を入力してください。")
    val input = io.StdIn.readLine()

    try{
      val num = input.toInt
      num
    }
    catch{
      case _: Exception =>
        println("数値を入力してください。")
        readInt(text)
    }
  }

  @tailrec
  def readDeckNum(): Int ={
    val num = readInt("使用する山札の個数")
    if (5 < num) readDeckNum()
    else num
  }

  def readHand(text: String): Hand ={
    val tramp: Tramp = {
      val input = readInt(s"${text}の手札")
      if (!(1 to 13).contains(input)) {
        println("1~13の数値を入力してください。")
        return readHand(text)
      }
      else input
    }
    new Hand(tramp)
  }

  def readBlackJackHand(text: String,deck: Deck): (Hand,Deck) ={
    val hand = readHand(text)
    val blackJackHand = {
      if (10 < hand.tramps.head) new Hand(10)
      else hand
    }
    (blackJackHand, deck diff blackJackHand.tramps)
  }

  @tailrec
  private def readDealerHitHand(hands: Seq[Hand], deck: Deck): (Seq[Hand],Deck) ={

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

  def readDealerHitHand(deck: Deck): (Seq[Hand],Deck) ={
    readDealerHitHand(Seq(), deck)
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
