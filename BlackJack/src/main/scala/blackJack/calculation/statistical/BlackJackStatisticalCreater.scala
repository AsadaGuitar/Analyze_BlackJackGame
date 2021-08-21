package blackjack.calculation.statistical

import blackjack._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class BlackJackStatisticalCreater(val hand: Hand, val deck: Deck, val timeOut: Duration)
  extends StatisticalCreater [Int]
  with PossibilityHandCalculator {

  val isInRange = (score: Int) => (17 to 21) contains score

  def joinTwoDimensions[T](ll: Seq[Seq[T]]): Seq[T] = for {l <- ll; r <- l} yield r

  @throws[InterruptedException]
  @throws[concurrent.TimeoutException]
  override def create() = {

    val futureResultList = parallelFindPossibility(hand,deck,isInRange)
    val resultList = Await.result(futureResultList,timeOut)
    val result = joinTwoDimensions(resultList)

    new BlackJackStatistical(hand,deck)(result)
  }
}

