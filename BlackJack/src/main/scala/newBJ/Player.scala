package newBJ

import scala.annotation.tailrec

class Player (h: Hand){
  require(h.nonEmpty)

  def this(t: Tramp) = this(Seq(t))

  private val _hand: Hand = h
  def hand: Hand = _hand

  def aceCount = hand.count(_ == 1)

  def hit(deck: Deck): Seq[(Deck,Hand)] =
    for{
      ts <- deck
    } yield (deck diff Seq(ts), hand :+ ts)

  def exchangeAce(isInRange: Int => Boolean): Player = {

    @tailrec
    def affect(hand: Hand, counter: Int)(fn: Hand => Hand): Hand =
      if (counter == 0) hand
      else affect(fn(hand), counter -1)(fn)

    @tailrec
    def find(hand: Hand, counter: Int): Hand ={
      if (counter == 0) return hand
      val deletedAce: Hand = affect(hand, counter)(x => x diff Seq(1))
      val addedEleven: Hand = affect(deletedAce, counter)(x => x :+ 11)

      if (isInRange(addedEleven.sum)) addedEleven
      else find(hand, counter -1)
    }

    new Player(find(hand,aceCount))
  }
}
