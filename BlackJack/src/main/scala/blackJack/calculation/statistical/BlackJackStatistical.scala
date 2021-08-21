package blackjack.calculation.statistical

import blackjack.Hand
import blackjack.calculation.statistical.probability.Rational
import blackjack._

class BlackJackStatistical(val userHand: Hand,
                           val deck: Deck)
                          (val set: Seq[Hand])
  extends ProbsStatistical[Int]{

  private def productOfList(ints: List[Int]): BigInt = {
    def loop(ints: List[Int]): BigInt = ints match {
      case Nil => 1
      case head :: tail => head * loop(tail)
    }
    if (ints.isEmpty) 0
    else loop(ints)
  }

  private def calculateProb(hand: Hand): Rational = {
    val r: List[Int] = (((deck.length - hand.length) + 1) to deck.length).toList
    new Rational(1, productOfList(r).toLong)
  }

  override def toList: Seq[(Int,Rational)] =
    for {h <- set} yield (h.sum, calculateProb(h))

  override def toMap: Map[Int,Rational] = {

    def createMap(probs: (Int,Rational), map: Map[Int,Rational]): Map[Int,Rational] =
      probs._1 match {
        case score if map.contains(score) =>
          val newProb = probs._2
          val oldProb = map(score)
          map.updated(score, newProb + oldProb)
        case _ => map.updated(probs._1, probs._2)
      }
    toList.foldLeft(Map(): Map[Int,Rational])((acc,x) => createMap(x,acc))
  }
}
