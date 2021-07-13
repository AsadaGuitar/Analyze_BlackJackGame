import Tramp._

object Main extends App {

  //ブラックジャックの最大点
  val BLACK_JACK: Int = 21
  //ディーラーが引くのを止める点
  val DEALERS_BORDER: Int = 16
  //ヒットする際判断の材料となるパーセンテージ
  val HIT_PERCENT: Double = 0.5
  //ゲームで使うトランプ
  val PLAYING_TRAMPS = Seq(Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Eleven, Twelve, Thirteen)

  //数字からトランプを取得
  val tramp: (Int,Seq[Tramp]) => Option[Tramp] =
    (num: Int, tramp: Seq[Tramp]) => tramp.find(_.num == num)

  //Aceを考慮したユーザーの手札を返却
  val handScore: (Tramp, Tramp) => Seq[Int] =
    (x: Tramp, y: Tramp) =>
      if(x == Ace || y == Ace) {
        if (x == Ace && y == Ace)
          Seq(12, 2)
        else Seq(x.num + y.num + 10)
      }
      else Seq(x.num + y.num)


  //山札の初期化
  def initDeck(deckNum: Int, tramps: Seq[Tramp]): Seq[Tramp] = {
    def addTramps(num: Int): Seq[Tramp] =
      if(num <= 1) tramps
      else tramps ++ addTramps(num - 1)

    if (deckNum < 1) Nil
    else addTramps(deckNum *4)
  }

  //山札からカードを削除
  def deleteTramp(tramp: Tramp, deck: Seq[Tramp]): Seq[Tramp] = deck.diff(Seq(tramp))

  //山札、もしくは全てのトランプから指定の範囲のトランプを検索
  //range.endが11の場合、Aceを含める
  def findTrampsByRange(range: Range, tramps: Seq[Tramp]): Seq[Tramp] ={
    val trampsInRange: Seq[Tramp] = tramps.filter(range contains _.num)
    if(range.end == 11) trampsInRange ++ tramps.filter(_ == Ace)
    else trampsInRange
  }

  //山札から指定のトランプを引く確率
  val ratioTrampsRangeInDeck: (Range,Seq[Tramp]) => Float =
    (range: Range, deck: Seq[Tramp]) => deck.count(range contains _.num) / deck.size.toFloat

  //得点と、最高得点を受取り、得点と合わせた時に最高得点を超えてしまう得点を返却
  val burstRange: (Int, Int) => Range =
    (score: Int, blackJack: Int) =>
      if(score >= blackJack) 1 to 11
      else blackJack - score + 1 to 11

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

  //各々が引いたカードを山札から削除
  deck = deleteTramp(tramp(userHandA,PLAYING_TRAMPS).get, deck)
  deck = deleteTramp(tramp(userHandB,PLAYING_TRAMPS).get, deck)
  deck = deleteTramp(tramp(dealerHand,PLAYING_TRAMPS).get, deck)

  println(deck)

  println(1 - ratioTrampsRangeInDeck(burstRange(userHandA+userHandB, BLACK_JACK), deck))

//  //手札の合計
//  val userTotal = userHandA + userHandB
//  println("userTotalNum : " + userTotal)
//
//  //バーストしないカード
//  println("ablePullCards : " + ablePullCards(userTotal,deck))
//  //バーストしないカードを引ける確率
//  println("rationNumbersInDeck : " + ratioNumbersInDeck(noBurstRange(userTotal),deck))
//  //Aceが1、11の時の山札の総和
//  println("averagePull : " + averagePull(deck))
//
//  //ディーラーの手札を平均点から予測
//  val dealerTotalA = addScoreUntilBorder(dealerHand,averagePull(deck)._1,DEALERS_BORDER)
//  val dealerTotalB = addScoreUntilBorder(dealerHand,averagePull(deck)._2,DEALERS_BORDER)
//  println("dealerTotalA : " + dealerTotalA)
//  println("deaderTotalB : " + dealerTotalB)
//
//  //ディーラーの手札が自分の手札よりも強い確率
//  val r = BLACK_JACK - (userTotal - dealerHand)

  //ユーザーがヒットした場合の手札の合計点
//  val nextUserTotal = userTotalNum + averageAblePull

  //ディーラーの手札の平均点
//  val dealerTotalHandsAverage = averageAblePullCardsPoints + dealerHand

//  if(userTotalNum < dealerTotalHandsAverage && ratioAblePullCards > HIT_PERCENT && isBurst(nextUserTotal)){
//    println("HIT")
//  }
//  else if(userTotalNum < delerTotalHandsAverage && raionAblePullCards < HIT_PERCENT){ //ここら辺修正
//    println("Surrender")
//  }
//  else println("Stand")
}