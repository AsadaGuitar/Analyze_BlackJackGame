object Main extends App {

  import BlackJack._

  val userHand = 18
  val tramps = Seq(1,2,3,4,5,6,7,8,9,10,10,10,10)

  val x = calculateAllHands(10, tramps)
  println(x)
  println(x.filter(21 < _._1)) //バーストしている手札
  val ratioDealerBurst = x.filter(21 < _._1).map(_._2).sum
  val ratioDealerWin = x.filter(x => userHand < x._1 && x._1 <= 21).map(_._2).sum
  val ratioDraw = x.filter(_._1 == userHand).map(_._2).sum

  println("ディーラがバーストする確率 : " + ratioDealerBurst)
  println("ディーラが勝つ確率 : " + ratioDealerWin)
  println("引き分けになる確率 : " + ratioDraw)
}
