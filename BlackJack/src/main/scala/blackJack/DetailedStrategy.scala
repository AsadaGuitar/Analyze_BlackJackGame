package blackJack

import blackJack.DetailedCalculation._

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

/*
name  DetailedStrategy
func  計算結果に基づきアクションを選択
 */
object DetailedStrategy {

  case class OriginalData(playerHands: Hands, dealerHands: Int, deck: Deck)

  case class StatisticalResults(probabilityMap: Map[Int,BigDecimal],calculationCounter: Long)

  case class CalculationProbabilityDetails(
                                            accuracy: BigDecimal,
                                            playerBurstProbability: Double,
                                            dealerBurstProbability: BigDecimal,
                                            dealerWinProbability: BigDecimal,
                                            dealerLoseProbability: BigDecimal,
                                            drawProbability: BigDecimal
                                          )

  val burstProbability: (Hands,Deck) => Double =
    (hands: Hands,deck: Deck) => {
      val count: Int = deck.count(x => 21 < (hands.sum + x))
      count.toDouble / deck.length
    }

  /*
  計算結果を標準出力
   */
  private def announceCalculationResults(results: CalculationProbabilityDetails): Unit ={
    val accuracy: Float = results.accuracy.toFloat
    val playerBurstProbability: Float = results.playerBurstProbability.toFloat
    val dealerBurstProbability: Float = results.dealerBurstProbability.toFloat
    val dealerWinProbability: Float = results.dealerWinProbability.toFloat
//    val dealerLoseProbability: Float = results.dealerLoseProbability.toFloat
    val drawProbability: Float = results.drawProbability.toFloat

    println(s"計算結果の正確性 : ${accuracy * 100}%")
    println(s"バースト確率[プレイヤー] : ${playerBurstProbability * 100}%")
    println(s"バースト確率[ディーラー] : ${dealerBurstProbability * 100}%")
    println(s"ディーラー勝率 : ${dealerWinProbability * 100}%")
    println(s"ドロー確率 : ${drawProbability * 100}%")
  }

  /*
  統計データに基づいた確率から次のアクションを選択
   */
  private def refinedJudge(results: CalculationProbabilityDetails): Action ={

    if(results.playerBurstProbability < 0.1) Hit
    else if (results.playerBurstProbability < 0.3 &&
      results.dealerWinProbability < results.dealerLoseProbability) DoubleDown
    else if (results.dealerWinProbability < results.dealerLoseProbability) Stand
    else if (results.dealerWinProbability > results.dealerLoseProbability) Hit
    else if (0.55 < results.drawProbability) Stand
    else Stand
  }

  def createPlayerScore(playerHands: Hands): Int ={
    val aceCount = playerHands.count(_==1)
    val changedHands = changeAceToEleven(aceCount,playerHands,x => (1 to 21).contains(x))
    changedHands match {
      case Some(x) => x.sum
      case None => playerHands.sum
    }
  }

  /*
  元のデータと予測結果の統計から確率を取得
   */
  private def createCalculationProbabilityDetails(originalData: OriginalData,
                                       statisticalResults: StatisticalResults): CalculationProbabilityDetails ={

    val probabilityMap = statisticalResults.probabilityMap

    val playerHands: Hands = originalData.playerHands
    val deck: Deck = originalData.deck
    val playerScore: Int = createPlayerScore(originalData.playerHands)

    val playerBurstProbability: Double = burstProbability(playerHands, deck)

    val accuracy: BigDecimal = probabilityMap.foldLeft(0: BigDecimal)((acc,x) => acc + x._2)

    val dealerBurstProbability: BigDecimal = probabilityMap(22)

    val dealerWinProbability: BigDecimal =
      probabilityMap
        .foldLeft(0: BigDecimal)((acc, x) => if (playerScore < x._1 && x._1 != 22) acc + x._2 else acc)

    val dealerLoseProbability: BigDecimal =
      probabilityMap
        .foldLeft(0: BigDecimal)((acc, x) => if (x._1 < playerScore || x._1 == 22) acc + x._2 else acc)

    val drawProbability: BigDecimal =
      probabilityMap
        .foldLeft(0: BigDecimal)((acc, x) => if (playerScore == x._1) acc + x._2 else acc)

    val results: CalculationProbabilityDetails = CalculationProbabilityDetails(
      accuracy,
      playerBurstProbability,
      dealerBurstProbability,
      dealerWinProbability,
      dealerLoseProbability,
      drawProbability
    )

    results
  }

  /*
  計算結果の統計を返却
   */
  @throws[concurrent.TimeoutException]
  @throws[InterruptedException]
  def createStatisticalResults(originalData: OriginalData): StatisticalResults ={

    def joinLists[T](ll: Seq[Seq[T]]): Seq[T] = for {l <- ll; r <- l} yield r

    val dealerHand = originalData.dealerHands
    val deck = originalData.deck
    val isInDealerRange: Int => Boolean = (hand: Int) => (17 to 21).contains(hand)

    println("計算を開始します。")
    val start = System.currentTimeMillis()
    val futureResult: Future[Seq[Seq[Rations]]] =
      asynchronousCalculationSeparated(
        Seq(dealerHand),
        deck,
        deck,
        isInDealerRange,
        futureLoopHit)
    val resultsList: Seq[Seq[Rations]] = Await.result(futureResult, 10 second)
    val end = System.currentTimeMillis()
    println("計算が完了しました。")
    println(s"所要時間 : ${(end - start) / 1000.toFloat}s")

    val results: Seq[Rations] = joinLists(resultsList)
    var probabilityMap: Map[Int, BigDecimal] = Map(17 -> 0, 18 -> 0, 19 -> 0, 20 -> 0, 21 -> 0, 22 -> 0)
    var calculationCounter: Long = 0

    for {
      r <- results
    } {
      calculationCounter = calculationCounter + r.calculatedCount

      if (probabilityMap.contains(r.tramp)) {
        val mapValue = probabilityMap(r.tramp)
        probabilityMap = probabilityMap.updated(r.tramp, mapValue + r.probability)
      }
      else {
        val mapValue = probabilityMap(22)
        probabilityMap = probabilityMap.updated(22, mapValue + r.probability)
      }
    }

    println(s"総計算回数 : ${calculationCounter}回")
    StatisticalResults(probabilityMap,calculationCounter)
  }

  /*
  入力値を受取り次のアクションを返却
   */
  @throws[concurrent.TimeoutException]
  @throws[InterruptedException]
  def getRefinedAction(originalData: OriginalData): Action = {

    val statisticalResults: StatisticalResults =
      createStatisticalResults(originalData)

    val calculationProbabilityDetails: CalculationProbabilityDetails =
      createCalculationProbabilityDetails(originalData,statisticalResults)

    announceCalculationResults(calculationProbabilityDetails)
    val action = refinedJudge(calculationProbabilityDetails)
    action
  }
}
