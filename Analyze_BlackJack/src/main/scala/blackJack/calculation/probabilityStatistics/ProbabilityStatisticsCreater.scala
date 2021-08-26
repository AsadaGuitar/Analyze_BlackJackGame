package blackJack.calculation.probabilityStatistics

import blackJack.{Deck, Hand}
import blackJack.calculation.Rational

trait ProbabilityStatisticsCreater[T] {

  def create(): ProbabilityStatistics[T]
}

class HandProbabilityStatisticsCreater(dealerHand: Hand, usingDeck: Deck) 
  extends ProbabilityStatisticsCreater[Int] 
    with HandProbabilityCalculator 
    with HandStatisticsCreater {

  private val _hand = dealerHand
  def hand = _hand

  private val _deck = usingDeck
  def deck = _deck
  
  override def create(): ProbabilityStatistics[Int] ={

    def probsList: Vector[(Int,Rational)] = 
      for {
        hand <- paralellLoopHit(hand, deck)
      } yield (hand.sum, calculateProb(hand,deck))

    probsList.foldLeft(new ProbabilityStatistics[Int]())((acc,x) => acc :+ x)
  }
}