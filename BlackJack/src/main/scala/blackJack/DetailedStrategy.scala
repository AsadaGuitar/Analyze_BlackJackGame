package blackJack

import blackJack.DetailedCalculation._

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.language.postfixOps


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
  統計データに基づいた確率から次のアクションを選択
   */
  private def refinedJudge(results: CalculationProbabilityDetails): Action ={

    println("Accuracy calculate Action is [" + results.accuracy + "]")
    println("playerBurstProbability : " + results.playerBurstProbability)
    println("dealerBurstProbability : " + results.dealerBurstProbability)
    println("dealerWinProbability : " + results.dealerWinProbability)
    println("dealerLoseProbability : " + results.dealerLoseProbability)
    println("drawProbability : " + results.drawProbability)

    if(results.playerBurstProbability < 0.1) Hit
    else if (results.playerBurstProbability < 0.3 &&
      results.dealerWinProbability < results.dealerLoseProbability) DoubleDown
    else if (results.dealerWinProbability < results.dealerLoseProbability) Stand
    else if (results.dealerWinProbability > results.dealerLoseProbability) Hit
    else if (0.55 < results.drawProbability) Stand
    else Stand
  }

  /*
  元のデータと予測結果の統計から確率を取得
   */
  private def createCalculationProbabilityDetails(originalData: OriginalData,
                                       statisticalResults: StatisticalResults): CalculationProbabilityDetails ={

    val probabilityMap = statisticalResults.probabilityMap

    val playerHands: Hands = originalData.playerHands
    val deck: Deck = originalData.deck
    val playerScore: Int = originalData.playerHands.sum

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

    val dealerHand = originalData.dealerHands
    val deck = originalData.deck

    val futureResult: Future[Seq[Seq[Rations]]] =
      asynchronousCalculationSeparated(
        Seq(dealerHand),
        deck,
        deck,
        isInDealerRange,
        futureLoopHit)

    val resultsList: Seq[Seq[Rations]] = Await.result(futureResult, 10 second)
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

    val action = refinedJudge(calculationProbabilityDetails)
    action
  }
}
