//import akka.actor.{Actor, ActorSystem, Props}

import scala.annotation.tailrec

object BlackJack {

  //リスト内の全ての要素同士で掛け算を行う
  private def productOfList(ints: List[Int]): Int = {
    def loop(ints: List[Int]): Int = ints match{
      case Nil => 1
      case head :: tail => head * loop(tail)
    }
    if (ints.isEmpty) 0
    else loop(ints)
  }

  //リストの長さからその手札の確率を算出
  val hitRatioByLength: (Int,Int) => Float = {
    (len: Int, deckLen: Int) =>
      val r = ((deckLen - len +2) to deckLen).toList
      1 / productOfList(r).toFloat
  }

  /*
  name    calculateAllHands
  func    17を超えるまで引く時、ディーラが辿る可能性のある全ての手札を取得し、その総和のリストを返却
  args    hands: Int        確認したディーラの手札
          deck: Seq[Int]    現在の山札
  return  Seq[Int]          ディーラが辿る可能性のある全ての手札
   */
  def calculateAllHands(hands: Int, parentDeck: Seq[Int]): Seq[(Int,Float)] = {

    //1を11にした方が良いかどうかの指標
    val isInRange: Int => Boolean = (hand: Int) => (17 to 21).contains(hand)

    /*
    name    changeAceToEleven
    func    1を11にした方が良い場合は受取ったリストに10を加えて返却、1のままの方が良い場合は何も返さない
    args    aceCount: Int       1の数
            hands: Seq[Int]     1が含まれている手札
    return  Option[Seq[Int]]    1を11に変えた手札の総和
     */
    @tailrec
    def changeAceToEleven(aceCount: Int, hands: Seq[Int]): Option[Seq[Int]] = {
      //aceCountが0の場合は変えない方が良いのでNoneを返却
      if (aceCount < 1) None
      //17<=x<=21の場合は変えた方が良いので、変えた分を加算リストを返却
      else if (isInRange(hands.sum + 10 * aceCount)) Some(hands :+ 10 * aceCount)
      //17<=x<=21に含まれていない場合は変える数を減らしてもう一度調べる
      else changeAceToEleven(aceCount -1, hands)
    }

    /*
    name    getHandsOver17
    func    手札を山札から引いていき、17以上の手札を返却する
    args    hands: Seq[Int]     1があるか確認する必要がある為、手札をリストで受取る
            deck: Seq[Int]      山札
    return  Seq[Int]            17を超えた手札
     */
    def getHandsOver17(hands: Seq[Int], deck: Seq[Int]): Seq[(Int,Float)] = {

      //受取った手札が17以上の場合はその総和とその手札を引く確率を返却
      if(17 <= hands.sum) {
        val score = hands.sum
        val ratio = hitRatioByLength(hands.length,parentDeck.length)
        Seq((score,ratio))
      }
      //手札に1が含まれていた場合
      else if(hands.contains(1)){
        //1を11に変えた方が良い場合は変えた方が良い数だけ10を足したリストを取得
        val changedAce = changeAceToEleven(hands.count(_ == 1), hands)
        changedAce match {
          //変えた方が良い場合はその総和を返却
          case Some(x) =>
            val score = x.sum
            //changeAceでリストの末尾に10を足しているので長さを1減らす
            val ratio = hitRatioByLength(x.length -1,parentDeck.length)
            Seq((score,ratio))
          //変えない方が良い場合は再帰
          case None =>
            for {
              //山札の全てのカードを取得
              t <- deck
              //手札に山札のカードを加え、山札から引いたカードを削除して再帰
              a <- getHandsOver17(hands :+ t, deck.diff(Seq(t)))
            } yield a
        }
      }
      //1が含まれておらず、17よりも低い場合
      else for {
        //山札の全てのカードを取得
        t <- deck
        a <- getHandsOver17(hands :+ t, deck.diff(Seq(t)))
        //手札に山札のカードを加え、山札から引いたカードを削除して再帰
      } yield a
    }

    //再帰関数の開始
    getHandsOver17(Seq(hands), parentDeck)
  }
}
