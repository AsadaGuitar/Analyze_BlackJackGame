package blackjack

import scala.annotation.tailrec

class Hand(ts: Tramps) {

  require{
    if(ts.nonEmpty) ts.foldLeft(true)((acc,x) => if (!((1 to 13) contains x)) false else acc)
    else true
  }

  private val _tramps = ts

  def tramps = _tramps

  def this() = this(Seq.empty)
  def this(t: Tramp) = this(Seq(t))

  def sum = tramps.sum

  def count(filter: Int => Boolean) = tramps.count(filter)

  def length = tramps.length

  def isExist(num: Int) = tramps.contains(num)

  def countAce = tramps.count(_==1)

  def exchangeAce(isInRange: Int => Boolean): Hand = {

    @tailrec
    def affect(ts: Tramps, counter: Int)(fn: Tramps => Tramps): Tramps =
      if (counter == 0) ts
      else affect(fn(ts), counter -1)(fn)

    @tailrec
    def exchange(counter: Int): Tramps ={
      if (counter == 0) return tramps
      val deletedAce = affect(tramps,counter)(x => x diff Seq(1))
      val addedEleven = affect(deletedAce,counter)(x => x :+ 11)

      if (isInRange(addedEleven.sum)) addedEleven
      else exchange(counter -1)
    }

    new Hand(exchange(countAce))
  }

  def +(that: Hand) = new Hand(that.tramps ++ tramps)

  def +(that: Int) = new Hand(that +: tramps)

  def -(that: Hand) = new Hand(tramps diff that.tramps)

  def -(that: Int) = new Hand(tramps diff Seq(that))
}
