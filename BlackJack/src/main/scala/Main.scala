import BlackJackIO._
import BlackJackCore._

object Main extends App {

  println("Welcome to \"BLACK JACK Operator\".")

//  val deckNum = readDeckNum()
//  val deck = initDeck(deckNum,TRAMPS)
//
  val userRight = readHand(User)
  val userLeft = readHand(User)
//  val userScore = userRight + userLeft
//
  val dealerHand = readHand(Dealer)
//
//  val x = calculateAllHandsHitInRange(
//    dealerHand, deck.diff(Seq(userRight,userLeft,dealerHand)))
//
//  val ratioDealerBurst = x.filter(21 < _._1).map(_._2).sum
//  val ratioDealerWin = x.filter(x => userScore < x._1 && x._1 <= 21).map(_._2).sum
//  val ratioDraw = x.filter(_._1 == userScore).map(_._2).sum
//
//  println("ユーザの手札 Right: %d, Left: %d".format(userRight,userLeft))
//  println("ディーラの手札 %d".formatted(dealerHand))
//  println("ディーラの総確率 : " + x.map(_._2).sum *100 + "%")
//  println("ディーラがバーストする確率 : " + ratioDealerBurst *100 + "%")
//  println("ディーラが勝つ確率 : " + ratioDealerWin *100 + "%")
//  println("引き分けになる確率 : " + ratioDraw *100 + "%")

  //****************************************************
  //develop

  val deck = initDeck(1,TRAMPS)

//  val userRight = 8
//  val userLeft = 10
  val userScore = userRight + userLeft

//  val dealerHand = 7

  val s = System.currentTimeMillis()

  val x = calculateAllHandsHitInRange(
    dealerHand, deck.diff(Seq({
      if(userRight == 11) 1
      else userRight
    }, {
      if(userLeft == 11) 1
      else userLeft
    },dealerHand)))

  val e = System.currentTimeMillis()

  println("時間 : " + (e - s))//10000//49067
//  println(x)
//  println(x.filter(21 < _._1)) //バーストしている手札

  val ratioDealerBurst = x.filter(21 < _._1).map(_._2).sum
  val ratioDealerWin = x.filter(x => userScore < x._1 && x._1 <= 21).map(_._2).sum
  val ratioDraw = x.filter(_._1 == userScore).map(_._2).sum

  println("ユーザの手札 Right: %d, Left: %d".format(userRight,userLeft))
  println("ディーラの手札 %d".formatted(dealerHand))
  println("ディーラの総確率 : " + x.map(_._2).sum *100 + "%")
  println("ディーラがバーストする確率 : " + ratioDealerBurst *100 + "%")
  println("ディーラが勝つ確率 : " + ratioDealerWin *100 + "%")
  println("引き分けになる確率 : " + ratioDraw *100 + "%")
}
