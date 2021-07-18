import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

object BlackJackCore {

  val TRAMPS = Seq(1,2,3,4,5,6,7,8,9,10,10,10,10)

  /*
  name    productOfList
  func    リスト内の全ての要素同士で掛け算を行う
  args    ints: List[Int]   計算を行うリスト
  return  リスト内の全ての要素同士で掛け算を行った結果
   */
  private def productOfList(ints: List[Int]): Int = {
    def loop(ints: List[Int]): Int = ints match{
      case Nil => 1
      case head :: tail => head * loop(tail)
    }
    if (ints.isEmpty) 0
    else loop(ints)
  }

  /*
  name    initDeck
  func    山札の数と使用するトランプを受取り、個数分のトランプをリストに格納し、返却
  args    deckNum: Int        使用する山札の数
          tramps: Seq[Int]    使用するトランプ
  return  Seq[Int]            使用する個数分を格納したリスト
   */
  def initDeck(deckNum: Int, tramps: Seq[Int]): Seq[Int] = {
    /*
    name    addTramps
    func    山札の数を受取り、その個数分のトランプを格納し返却
    args    num: Int     山札の個数
    return  Seq[Int]     山札の個数分を格納したリスト
     */
    def addTramps(num: Int): Seq[Int] = {
      //求める山札の個数が1未満の場合、トランプのセットを返却
      if(num <= 1) tramps
      //トランプを格納し再帰
      else tramps ++ addTramps(num - 1)
    }

    //求める山札の個数が1未満の場合、空のリストを返す
    if (deckNum < 1) Nil
    //求める山札の個数分の山札を用意
    else addTramps(deckNum *4)
  }


  /*
  name    hitRatioByLength
  func    リストの長さからその手札を引く確率を算出
  args    len: Int        リスト(手札)の長さ
          deckLen: Int    初めに受け取ったリストの長さ
  return  Float           lenの長さを算出したリストを引く確率
   */
  val hitRatioByLength: (Int,Int) => Float = {
    (len: Int, deckLen: Int) =>
      val r = ((deckLen - len +2) to deckLen).toList
      1 / productOfList(r).toFloat
  }

  /*
  name    calculateAllHands
  func    17を超えるまで引く時、ディーラが辿る可能性のある全ての手札を取得し、その総和と引く確率のタプルのリストを返却
  args    hands: Int        確認したディーラの手札
          deck: Seq[Int]    現在の山札
  return  Seq[(Int,Float)]
          Int               引く可能性がある手札の総和
          Float             その確率
   */
  def calculateAllHandsHitInRange(hands: Int, parentDeck: Seq[Int]): Seq[(Int,Float)] = {

    /*
    name    isInRange
    func    1を含む手札の総和を受取り、その総和に10を足した数値が17～21に含まれていればtrueを返却
    args    hand: Int   1が含まれている手札の総和
    return  Boolean     1を11にした方が良い場合にtrueを返却
     */
    val isInRange: Int => Boolean = (hand: Int) => (17 to 21).contains(hand)

    /*
    name    changeAceToEleven
    func    1を11にした方が良い場合は受取ったリストの末尾に10を加えて返却、1のままの方が良い場合は何も返さない
    args    aceCount: Int       1が含まれている数
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
    return  Seq[(Int,Float)]
            Int                 引く可能性がある手札の総和
            Float               その確率
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
        //1を11に変えた方が良い場合は変えた方が良い数だけ末尾に10を足したリストを取得
        val changedAce = changeAceToEleven(hands.count(_ == 1), hands)
        changedAce match {
          //変えた方が良い場合はその総和を返却
          case Some(x) =>
            val score = x.sum
            //changeAceでリストの末尾に、必要分だけ10を掛けた値を足しているので長さを1減らす
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


  //***************************************************************************************

//
//
//  /*
//  name    calculateAllHands
//  func    17を超えるまで引く時、ディーラが辿る可能性のある全ての手札を取得し、その総和と引く確率のタプルのリストを返却
//  args    hands: Int        確認したディーラの手札
//          deck: Seq[Int]    現在の山札
//  return  Seq[(Int,Float)]
//          Int               引く可能性がある手札の総和
//          Float             その確率
//   */
//  def calculateAllHandsHitInRange(hands: Int, parentDeck: Seq[Int]): Seq[(Int,Float)] = {
//
//    /*
//    name    isInRange
//    func    1を含む手札の総和を受取り、その総和に10を足した数値が17～21に含まれていればtrueを返却
//    args    hand: Int   1が含まれている手札の総和
//    return  Boolean     1を11にした方が良い場合にtrueを返却
//     */
//    val isInRange: Int => Boolean = (hand: Int) => (17 to 21).contains(hand)
//
//    /*
//    name    changeAceToEleven
//    func    1を11にした方が良い場合は受取ったリストの末尾に10を加えて返却、1のままの方が良い場合は何も返さない
//    args    aceCount: Int       1が含まれている数
//            hands: Seq[Int]     1が含まれている手札
//    return  Option[Seq[Int]]    1を11に変えた手札の総和
//     */
//    @tailrec
//    def changeAceToEleven(aceCount: Int, hands: Seq[Int]): Option[Seq[Int]] = {
//      //aceCountが0の場合は変えない方が良いのでNoneを返却
//      if (aceCount < 1) None
//      //17<=x<=21の場合は変えた方が良いので、変えた分を加算リストを返却
//      else if (isInRange(hands.sum + 10 * aceCount)) Some(hands :+ 10 * aceCount)
//      //17<=x<=21に含まれていない場合は変える数を減らしてもう一度調べる
//      else changeAceToEleven(aceCount -1, hands)
//    }
//
//    /*
//    name    getHandsOver17
//    func    手札を山札から引いていき、17以上の手札を返却する
//    args    hands: Seq[Int]     1があるか確認する必要がある為、手札をリストで受取る
//            deck: Seq[Int]      山札
//    return  Seq[(Int,Float)]
//            Int                 引く可能性がある手札の総和
//            Float               その確率
//     */
//    def getHandsOver17(hands: Seq[Int], deck: Seq[Int]): Seq[(Int,Float)] = {
//
//      //受取った手札が17以上の場合はその総和とその手札を引く確率を返却
//      if(17 <= hands.sum) {
//        val score = hands.sum
//        val ratio = hitRatioByLength(hands.length,parentDeck.length)
//        Seq((score,ratio))
//      }
//      //手札に1が含まれていた場合
//      else if(hands.contains(1)){
//        //1を11に変えた方が良い場合は変えた方が良い数だけ末尾に10を足したリストを取得
//        val changedAce = changeAceToEleven(hands.count(_ == 1), hands)
//        changedAce match {
//          //変えた方が良い場合はその総和を返却
//          case Some(x) =>
//            val score = x.sum
//            //changeAceでリストの末尾に、必要分だけ10を掛けた値を足しているので長さを1減らす
//            val ratio = hitRatioByLength(x.length -1,parentDeck.length)
//            Seq((score,ratio))
//          //変えない方が良い場合は再帰
//          case None =>
//            for {
//              //山札の全てのカードを取得
//              t <- deck
//            }
//        }
//      }
//      //1が含まれておらず、17よりも低い場合
//      else for {
//        //山札の全てのカードを取得
//        t <- deck
//        f <- {
//          val f = Future(getHandsOver17(hands :+ t, deck.diff(Seq(t))))
//          Await.result(f, Duration.Inf)
//        }
//      } yield f
//    }
//
//    //再帰関数の開始
//    getHandsOver17(Seq(hands), parentDeck)
//  }
}
