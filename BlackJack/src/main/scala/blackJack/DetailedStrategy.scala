package blackJack

import blackJack.DetailedCalculation._

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

/*
オブジェクト名   DetailedStrategy
機能    　  計算結果に基づきアクションを選択
 */
object DetailedStrategy {

  /*
  クラス名    OriginalData
  機能       初期情報を格納
  メンバ     playerHands: Hands    プレイヤーの手札
            dealerHands: Int      ディーラの手札
            deck: Deck            使用している山札
   */
  case class OriginalData(playerHands: Hands, dealerHands: Int, deck: Deck)

  /*
  クラス名    StatisticalResults
  機能       計算結果の統計情報を格納
  メンバ     probabilityMap: Map[Int,BigDecimal]   各スコアの確率
            calculationCounter: Long              計算回数
   */
  case class StatisticalResults(probabilityMap: Map[Int,BigDecimal],calculationCounter: Long)

  /*
  クラス名    CalculationProbabilityDetails
  機能       勝率などの統計情報を格納
  メンバ      accuracy: BigDecimal                   計算の正確性
             playerBurstProbability: Double         プレイヤーがバーストする確率
             dealerBurstProbability: BigDecimal     ディーラがバーストする確率
             dealerWinProbability: BigDecimal       ディーラが勝利する確率
             dealerLoseProbability: BigDecimal      ディーラが敗北する確率
             drawProbability: BigDecimal            引き分けになる確率
   */
  case class CalculationProbabilityDetails(
                                            accuracy: BigDecimal,
                                            playerBurstProbability: Double,
                                            dealerBurstProbability: BigDecimal,
                                            dealerWinProbability: BigDecimal,
                                            dealerLoseProbability: BigDecimal,
                                            drawProbability: BigDecimal
                                          )

  /*
  関数名   burstProbability
  機能     手札と山札からバーストする確率を計算
  引数     hands: Hands   手札
          deck: Deck     山札
  返値     Double         バーストする確率
   */
  val burstProbability: (Hands,Deck) => Double =
    (hands: Hands,deck: Deck) => {
      //手札と足して21を超えるトランプの枚数を格納
      val count: Int = deck.count(x => 21 < (hands.sum + x))
      count.toDouble / deck.length
    }

  /*
  メソッド名   announceCalculationResults
  機能        計算結果を標準出力
  引数        results: CalculationProbabilityDetails    計算結果の統計情報
   */
  private def announceCalculationResults(results: CalculationProbabilityDetails): Unit ={
    //表示する際はFloatにする
    val accuracy: Float = results.accuracy.toFloat
    val playerBurstProbability: Float = results.playerBurstProbability.toFloat
    val dealerBurstProbability: Float = results.dealerBurstProbability.toFloat
    val dealerWinProbability: Float = results.dealerWinProbability.toFloat
    val dealerLoseProbability: Float = results.dealerLoseProbability.toFloat
    val drawProbability: Float = results.drawProbability.toFloat
    //標準出力
    println(s"計算結果の正確性 : ${accuracy * 100}%")
    println(s"バースト確率[プレイヤー] : ${playerBurstProbability * 100}%")
    println(s"バースト確率[ディーラー] : ${dealerBurstProbability * 100}%")
    println(s"ディーラー勝率 : ${dealerWinProbability * 100}%")
    println(s"ディーラー負率 : ${(dealerLoseProbability + dealerBurstProbability) * 100}%")
    println(s"ドロー確率 : ${drawProbability * 100}%")
  }

  /*
  メソッド名   refinedJudge
  機能        統計データに基づいた確率から次のアクションを選択
  引数        results: CalculationProbabilityDetails    計算結果の統計情報
  返値        Action                                    統計情報に基づき選択された最善手
   */
  private def refinedJudge(results: CalculationProbabilityDetails): Action ={

    //バーストする確率が10%未満の場合Hitを選択
    if(results.playerBurstProbability < 0.1) Hit
    else if (results.playerBurstProbability < 0.3 &&
      results.dealerWinProbability < results.dealerLoseProbability) DoubleDown
    else if (results.dealerWinProbability < results.dealerLoseProbability) Stand
    else if (results.dealerWinProbability > results.dealerLoseProbability) Hit
    else if (0.55 < results.drawProbability) Stand
    else Stand
  }

  /*
  メソッド名   createPlayerScore
  機能        プレイヤーの手札から最高点の組み合わせを作成
  引数        playerHands: Hands      プレイヤーの手札
  返値        Int                     プレイヤーの手札の最高点
   */
  def createPlayerScore(playerHands: Hands): Int ={
    //Aceのカウント
    val aceCount = playerHands.count(_==1)
    //Aceを11に変えた時、17~21内に収まっているか
    val changedHands: Option[Hands] = changeAceToEleven(aceCount,playerHands,x => (17 to 21).contains(x))
    changedHands match {
      //17~21内に収まっている場合
      case Some(x) => x.sum
      //17~21内に収まっていない場合
      case None => playerHands.sum
    }
  }

  /*
  メソッド名   createCalculationProbabilityDetails
  機能        元のデータと予測結果の統計から確率を取得
   */
  private def createCalculationProbabilityDetails(originalData: OriginalData,
                                       statisticalResults: StatisticalResults): CalculationProbabilityDetails ={

    val probabilityMap = statisticalResults.probabilityMap

    val playerHands: Hands = originalData.playerHands
    val deck: Deck = originalData.deck
    val playerScore: Int = createPlayerScore(originalData.playerHands)

    //計算の正確性
    val accuracy: BigDecimal = probabilityMap.foldLeft(0: BigDecimal)((acc,x) => acc + x._2)
    //プレイヤーがバーストする確率
    val playerBurstProbability: Double = burstProbability(playerHands, deck)
    //ディーラがバーストする確率
    val dealerBurstProbability: BigDecimal = probabilityMap(22)
    //ディーラが勝利する確率
    val dealerWinProbability: BigDecimal =
      probabilityMap
        .foldLeft(0: BigDecimal)((acc, x) => if (playerScore < x._1 && x._1 != 22) acc + x._2 else acc)
    //ディーラが敗北する確率
    val dealerLoseProbability: BigDecimal =
      probabilityMap
        .foldLeft(0: BigDecimal)((acc, x) => if (x._1 < playerScore || x._1 == 22) acc + x._2 else acc)
    //引き分けになる確率
    val drawProbability: BigDecimal =
      probabilityMap
        .foldLeft(0: BigDecimal)((acc, x) => if (playerScore == x._1) acc + x._2 else acc)
    //結果を格納
    val results: CalculationProbabilityDetails = CalculationProbabilityDetails(
      accuracy,
      playerBurstProbability,
      dealerBurstProbability,
      dealerWinProbability,
      dealerLoseProbability,
      drawProbability)
    //結果を返却
    results
  }

  /*
  メソッド名   createStatisticalResults
  機能        計算結果の統計を返却
  引数        originalData: OriginalData    初期情報
  返値        StatisticalResults            計算結果の統計情報
   */
  @throws[concurrent.TimeoutException]
  @throws[InterruptedException]
  def createStatisticalResults(originalData: OriginalData): StatisticalResults ={
    //二次元リストをリストに変換する関数
    def joinLists[T](ll: Seq[Seq[T]]): Seq[T] = for {l <- ll; r <- l} yield r

    val dealerHand = originalData.dealerHands
    val deck = originalData.deck
    val isInDealerRange: Int => Boolean = (hand: Int) => (17 to 21).contains(hand)

    println("計算を開始します。")
    //開始時間を計測
    val start = System.currentTimeMillis()
    //処理開始
    val futureResult: Future[Seq[Seq[Rations]]] =
      asynchronousCalculationSeparated(
        Seq(dealerHand),
        deck,
        deck,
        isInDealerRange)
    //10秒後に取り出す。処理が完了していない場合エラーになる
    val resultsList: Seq[Seq[Rations]] = Await.result(futureResult, 10 second)
    //終了時間を計測
    val end = System.currentTimeMillis()
    println("計算が完了しました。")
    println(s"所要時間 : ${(end - start) / 1000.toFloat}s")

    //リストの結合
    val results: Seq[Rations] = joinLists(resultsList)
    //統計情報の初期化
    var probabilityMap: Map[Int, BigDecimal] = Map(17 -> 0, 18 -> 0, 19 -> 0, 20 -> 0, 21 -> 0, 22 -> 0)
    var calculationCounter: Long = 0

    //統計情報の書込
    for {
      r <- results
    } {
      //計算回数を取得
      calculationCounter = calculationCounter + r.calculatedCount
      //Mapにスコアが存在する場合
      if (probabilityMap.contains(r.score)) {
        val mapValue = probabilityMap(r.score)
        probabilityMap = probabilityMap.updated(r.score, mapValue + r.probability)
      }
      //Mapにスコアが存在しない場合、22に書込み
      else {
        val mapValue = probabilityMap(22)
        probabilityMap = probabilityMap.updated(22, mapValue + r.probability)
      }
    }
    //計算回数を出力
    println(s"総計算回数 : ${calculationCounter}回")
    StatisticalResults(probabilityMap,calculationCounter)
  }

  /*
  メソッド名   getRefinedAction
  機能        入力値を受取り次のアクションを返却
  引数        originalData: OriginalData    初期情報
  戻値        Action                        計算された最善手
   */
  @throws[concurrent.TimeoutException]
  @throws[InterruptedException]
  def getRefinedAction(originalData: OriginalData): Action = {
    //計算結果の統計情報を取得
    val statisticalResults: StatisticalResults =
      createStatisticalResults(originalData)
    //確率を取得
    val calculationProbabilityDetails: CalculationProbabilityDetails =
      createCalculationProbabilityDetails(originalData,statisticalResults)
    //確率を標準出力
    announceCalculationResults(calculationProbabilityDetails)
    //アクションを取得
    val action = refinedJudge(calculationProbabilityDetails)
    action
  }
}
