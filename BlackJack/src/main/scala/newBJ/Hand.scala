package newBJ

class Hand (ts: Tramps) {

  require(ts.nonEmpty)
  require((1 to 13) contains ts)

  private val _tramps = ts
  def tramps = _tramps

  def sum = tramps.sum

  def count(filter: Int => Boolean) = tramps.count(filter)

  def length = tramps.length

  def +(that: Hand) = new Hand(that.tramps ++ tramps)

  def +(that: Int) = new Hand(that +: tramps)

  def -(that: Hand) = new Hand(tramps diff that.tramps)

  def -(that: Int) = new Hand(tramps diff Seq(that))

}
