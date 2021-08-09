import scala.annotation.tailrec

package object blackJack {

  type Deck = Seq[Int]
  type Hands = Seq[Int]

  abstract class Action
  case object Hit extends Action
  case object Stand extends Action
  case object DoubleDown extends Action

  abstract class GameCommand
  case object StartGame extends GameCommand
  case object Continue extends GameCommand
  case object InitDeck extends GameCommand
  case object Finish extends GameCommand

  abstract class CalculationType
  case object Simplified extends CalculationType
  case object Refined extends CalculationType

  val TRAMPS = Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10)

  private def productOfList(ints: List[Int]): BigInt = {
    def loop(ints: List[Int]): BigInt = ints match {
      case Nil => 1
      case head :: tail => head * loop(tail)
    }
    if (ints.isEmpty) 0
    else loop(ints)
  }

  val hitRatioByLengthCalculation: (Int, Int) => BigDecimal = {
    (len: Int, deckLen: Int) =>
      val r: List[Int] = (((deckLen - len) + 1) to deckLen).toList
      1 / productOfList(r).toDouble
  }

  val denominatorCalculation: (Int, Int) => BigInt = {
    (len: Int, deckLen: Int) =>
      val r: List[Int] = (((deckLen - len) + 1) to deckLen).toList
      val denominator = productOfList(r)
      denominator
  }

  def initDeck(deckNum: Int, tramps: Seq[Int]): Deck = {
    def addTramps(num: Int, ts: Seq[Int]): Seq[Int] = {
      if (num <= 1) ts
      else ts ++ addTramps(num - 1, ts)
    }
    if (deckNum < 1) Nil
    else addTramps(deckNum * 4, tramps)
  }

  def deleteHandsAtDeck(deck: Deck,hand: Int): Deck ={
    deck.diff(Seq(hand))
  }

  @tailrec
  def changeAceToEleven(aceCount: Int, hands: Hands, isInRange: Int => Boolean): Option[Hands] =
    if (aceCount < 1) None
    else if ((17 to 21).contains(hands.sum + 10 * aceCount)) Some(hands :+ 10 * aceCount)
    else changeAceToEleven(aceCount - 1, hands, isInRange)
}
