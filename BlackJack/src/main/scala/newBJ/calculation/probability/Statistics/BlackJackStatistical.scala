package newBJ.calculation.probability.Statistics

import newBJ.calculation.probability.Rational
import newBJ.calculation.probability.Statistics.Statistical.productOfList
import newBJ.{Deck, Hand}

object BlackJackStatistical {

  private def calculateProbability(hand: Hand,deck: Deck): Rational = {
    val r: List[Int] = (((deck.length - hand.length) + 1) to deck.length).toList
    new Rational(1, productOfList(r).toLong)
  }

  def entrySet(hands: Seq[Hand], deck: Deck): BlackJackStatistical ={

    def createMap(probs: (Int,Rational), map: Map[Int,Rational]): Map[Int,Rational] =
      probs._1 match {
        case score if map.contains(score) =>
          val newProb = probs._2
          val oldProb = map(score)
          map.updated(score, newProb + oldProb)
        case _ => map.updated(probs._1, probs._2)
      }

    val probsList: Seq[(Int, Rational)] =
      for {h <- hands} yield (h.sum, calculateProbability(h,deck))

    val probsMap = probsList.foldLeft(Map(): Map[Int,Rational])((acc,x) => createMap(x,acc))
    new BlackJackStatistical(probsMap)
  }
}

class BlackJackStatistical(probsMap: Map[Int,Rational])
  extends Statistical[Int](probsMap) with ProbabilityOfBlackJack {

  override def accuracyProb = map ?= (_ => true)

  override def dealerBurstProb = map ?= (21 < _)

  override def dealerWinProb(userScore: Int) = map ?= (x => x <= 21 && userScore < x)

  override def dealerLoseProb(userScore: Int) = map ?= (x => 21 < x && x < userScore)

  override def userBurstProb(userScore: Int, deck: Deck) = {
    val count: Int = deck.count(x => 21 < userScore + x)
    Some(new Rational(count, deck.length))
  }

  override def userWinProb(userScore: Int) = map ?= (x => 21 < x && x < userScore)

  override def drawProb(userScore: Int) = map ?= (_ == userScore)
}
