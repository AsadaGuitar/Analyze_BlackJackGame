package newBJ

import scala.annotation.tailrec

class Player (val h: Option[Hand]){

  def this(h: Hand) = this(Some(h))
  def this() = this(None)

  private val _hand = h
  def hand = _hand

  def set(h: Hand) = new Player(h)

  def countAce = hand match {
    case Some(x) => x.tramps.count(_==1)
    case None => 0
  }

  def exchangeAce(isInRange: Int => Boolean): Player = {

    @tailrec
    def affect(hand: Hand, counter: Int)(fn: Hand => Hand): Hand =
      if (counter == 0) hand
      else affect(fn(hand), counter -1)(fn)

    @tailrec
    def exchange(hand: Hand, counter: Int): Hand ={
      if (counter == 0) return hand
      val deletedAce = affect(hand, counter)(x => x - 1)
      val addedEleven = affect(deletedAce, counter)(x => x + 11)

      if (isInRange(addedEleven.sum)) addedEleven
      else exchange(hand, counter -1)
    }

    hand match {
      case Some(x) => new Player(exchange(x,countAce))
      case None => new Player()
    }
  }
}
