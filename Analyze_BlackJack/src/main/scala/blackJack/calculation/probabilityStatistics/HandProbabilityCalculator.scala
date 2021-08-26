package blackJack.calculation.probabilityStatistics

import blackJack.{Deck, Hand}
import blackJack.calculation.Rational

object HandProbabilityCalculator {

  def productOfList(ints: List[Int]): BigInt = {
    def loop(ints: List[Int]): BigInt = ints match {
      case Nil => 1
      case head :: tail => head * loop(tail)
    }
    if (ints.isEmpty) 1
    else loop(ints)
  }
}

trait HandProbabilityCalculator {
  import HandProbabilityCalculator._

  def calculateProb(hand: Hand,deck: Deck): Rational = {
    val r: List[Int] = (((deck.length - hand.length) + 2) to deck.length).toList
    new Rational(1, productOfList(r).toLong)
  }
}
