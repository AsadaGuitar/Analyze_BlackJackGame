import Tramp._

object Main extends App {

  val BLACK_JACK: Int = 21
  val DEALERS_BORDER: Int = 16
  val HIT_PERCENT: Double = 0.5
  val PLAYING_TRAMPS = Seq(Ace,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Eleven,Twelve,Thirteen)

  def isOver(trampNum: Int, border: Int): Boolean = border < trampNum

  def initDeck(num: Int, tramps: Seq[AbsTramp]): Map[AbsTramp,Int] ={
    val deck = tramps.foldLeft(Map[AbsTramp, Int]()) {(m, s) => m ++ Map(s -> 4)}
    if(num == 1) return deck
    deck.map(x => (x._1, x._2 * num))
  }

  val getTramp: (Int,Seq[AbsTramp]) => AbsTramp
  = (num: Int, playingTramps: Seq[AbsTramp]) => playingTramps.filter(t => t.num == num).head

  def deleteTramp(tramp: AbsTramp, deck: Map[AbsTramp,Int]): Map[AbsTramp,Int] ={
    if(deck(tramp) == 0) throw new IllegalStateException("カード枚数が負数です。")
    deck.updated(tramp, deck(tramp) -1)
  }

  //*******************************************************

  println("デッキの数を入力してください。")
  val numberOfDeck = io.StdIn.readInt()
  var deck = initDeck(numberOfDeck,PLAYING_TRAMPS)

  println("あなたの手札を教えてください。")
  println("一枚目 : ")
  val userHandA = io.StdIn.readInt()
  println("二枚目 : ")
  val userHandB = io.StdIn.readInt()

  println("ディーラーの手札を教えてください。")
  val dealerHand = io.StdIn.readInt()

  deck = deleteTramp(getTramp(userHandA,PLAYING_TRAMPS),deck)
  deck = deleteTramp(getTramp(userHandB,PLAYING_TRAMPS),deck)
  deck = deleteTramp(getTramp(dealerHand,PLAYING_TRAMPS),deck)

  println("deck : " + deck)

  //手札の合計
  val userTotalNum = userHandA + userHandB
  println("userTotalNum : " + userTotalNum)

  //手札と足して21を超えない数値を代入
  val cardsRangeAblePull: Range = 1 to BLACK_JACK - userTotalNum
  println("cardsRangeAblePull : " + cardsRangeAblePull)

  //山札に存在するカードの中で、手札と足して21を超えないカードの数値を代入
  val cardsRangeAblePullInTramp: Range = {
    if(cardsRangeAblePull.end > 10) 1 to 10
    else cardsRangeAblePull
  }
  println("cardsRangeAblePullInTramp : " + cardsRangeAblePullInTramp)

  //手札と足して21を超えない全てのカードを代入
  val ablePullCardsMapSeq: Seq[Map[AbsTramp,Int]] = for(c <- cardsRangeAblePullInTramp) yield deck.filter(_._1.num == c)
  println("ablePullCardsMapSeq : " + ablePullCardsMapSeq)

  //手札と足して21を超えない全てのカードの残り枚数を代入
  val ablePullCardsCounts: Seq[Int] = for(m <- ablePullCardsMapSeq) yield m.foldLeft(0)((acc,x) => acc + x._2)
  println("ablePullCardsCounts : " + ablePullCardsCounts)

  //山札の中で、手札と足して21を超えないカードの総和の割合
  println("ablePullCardsCounts.sum : " + ablePullCardsCounts.sum)
  println("deck.values.sum : " + deck.values.sum)
  val ratioAblePullCards: Double = ablePullCardsCounts.sum / deck.values.sum.toFloat
  println("ratioAblePullCards : " + ratioAblePullCards)
}


