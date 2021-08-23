package blackjack.system.parts

import blackjack.calculation.statistical.{BlackJackStatisticalCreater, ProbsStatistical}
import blackjack.{Deck, Hand}

import scala.concurrent.duration.Duration

trait Computable {

  def calculate(dealerHand: Hand,
                deck: Deck)
               (implicit timeOut: Duration): Option[ProbsStatistical[Int]] = {

    val statisticalCreate = new BlackJackStatisticalCreater(dealerHand, deck, timeOut)

    try {
      val statistical = statisticalCreate.create()
      Some(statistical)
    }
    catch {
      case _: Exception =>
        None
    }
  }
}
