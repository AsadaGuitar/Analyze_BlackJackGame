import Tramp._


object Main extends App {

  class PipelineHelper[T](x: T) {
    def |>[S](f: T => S): S = f(x)
  }
  implicit def Pipeline[T](x: T): PipelineHelper[T] = new PipelineHelper(x)

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
        else Seq(x.num + y.num + 10, x.num + y.num)
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
  val overRange: (Int, Int) => Range =
    (score: Int, blackJack: Int) =>
      if(score >= blackJack) 1 to 11
      else blackJack - score + 1 to 11

  //得点と山札を受取りバーストしない確率を返却
  val ratioNoBurst: (Int, Int, Seq[Tramp]) => Float =
    (hand: Int, max: Int, deck: Seq[Tramp]) => 1 - ratioTrampsRangeInDeck(overRange(hand, max), deck)

  //Trampの集合からAceが1の時と11の時の平均値を取得
  val trampsAverage: Seq[Tramp] => Seq[Float] =
    (tramps: Seq[Tramp]) => {
      val maxNums = tramps.foldLeft(0)((acc,x) => acc + (if (x == Ace) 11 else x.num))
      val minNums = tramps.foldLeft(0)((acc,x) => acc + (if (x == Ace) 1 else x.num))
      val lengthF = tramps.length.toFloat
      Seq(minNums / lengthF, maxNums / lengthF)
    }

  //指定の範囲に到達するまで引き、その度に山札から手札を消す
  def calculateRatioUntilReachRange(score: Int, deck: Seq[Tramp], range: Range): Seq[Int] ={

    def getAddition(num: Int, numDeck: Seq[Int]): Seq[(Seq[Int], Seq[Tramp])] ={
      for{
        t <- numDeck
      } yield {
        val l = {
          if(t == 1) {
            if(range.start < num +11){
              if(range.end < num +11 ) Seq(num +1)
              else Seq(num +11)
            }
            Seq(num +1, num +11)
          }
          else Seq(num +t)
        }
        (l, deleteTramp(tramp(t,PLAYING_TRAMPS).get, numDeck.map(x => tramp(x,PLAYING_TRAMPS).get)))
      }
    }

    def addUntilRange(n: Int, deck: Seq[Tramp]): Seq[Int] = n match {
      case a if range.start <= a => Seq(a)
      case _ =>
        for{
          (a,b) <- getAddition(n, deck.map(_.num))
          c <- a
          y <- addUntilRange(c, b)
        } yield y
    }
    addUntilRange(score,deck)
  }


  //*************************************************************************************

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


  //バーストしない確率（ディーラー）
  val dealerNext = for{
    x <- trampsAverage(deck)
  } yield x + dealerHand
  println("ディーラの合計が16以下の確率 : " + ratioNoBurst(dealerHand,DEALERS_BORDER,deck))
  println("ディーラ合計(期待値) : " + dealerNext)
  //バーストしない確率（ユーザー）
  for{
    x <- handScore(tramp(userHandA, PLAYING_TRAMPS).get,tramp(userHandB, PLAYING_TRAMPS).get)
    y <- handScore(tramp(dealerHand, PLAYING_TRAMPS).get,tramp(trampsAverage(deck)(1).toInt, PLAYING_TRAMPS).get)
  }yield {
    println("ユーザがHitしてバーストしない確率")
    println(ratioNoBurst(x, BLACK_JACK, deck))
    println("ディーラがHItしてバーストしない確率")
    println(ratioNoBurst(y,BLACK_JACK,deck))
  }

  val overRangeScores = calculateRatioUntilReachRange(dealerHand, deck,17 to 21)
  val inRangeScores = overRangeScores.filter(_ <= 21)
  println("ディーラーがこのゲームでバーストする確率 : " + (1 - (inRangeScores.length / overRangeScores.length.toFloat)))
  println(overRangeScores.length)



  println("deckの平均値 : " + trampsAverage(deck))

//  println(deck.map(_.num))

//  println("************\n" + calculateRatioUntilReachRange(10,deck,17 to 21))


//  val overRangeScoresU = calculateRatioUntilReachRange(userHandA + userHandB, deck, 17 to 21)
//  val inRangeScoresU = overRangeScores.filter(x => 17 <= x && x <= 21)
//  println("ユーザーがこのゲームでオーバーする確率 : " + (1 - (inRangeScoresU.length / overRangeScoresU.length.toFloat)))





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