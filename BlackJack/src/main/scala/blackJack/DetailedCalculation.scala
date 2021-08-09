package blackJack

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object DetailedCalculation {

  case class Rations(tramp: Int, probability: BigDecimal, calculatedCount: Long)

  val isInDealerRange: Int => Boolean = (hand: Int) => (17 to 21).contains(hand)

  def joinLists[T](ll: Seq[Seq[T]]): Seq[T] = for {l <- ll; r <- l} yield r

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

  @tailrec
  def changeAceToEleven(aceCount: Int, hands: Hands, isInRange: Int => Boolean): Option[Seq[Int]] =
    if (aceCount < 1) None
    else if ((17 to 21).contains(hands.sum + 10 * aceCount)) Some(hands :+ 10 * aceCount)
    else changeAceToEleven(aceCount - 1, hands, isInRange)

  val loopHit: (Hands,Deck,Deck,Int => Boolean,Long) => Seq[Rations] =
    (hands: Hands,
     deck: Deck,
     parentDeck: Deck,
     hitRange: Int => Boolean,
     calculatedCount: Long) => {

      val score: Int = hands.sum
      val count: Long = calculatedCount + 1

      if (17 <= score) {
        val denominator = hitRatioByLengthCalculation(hands.length -1, parentDeck.length)
        Seq(Rations(score,denominator,count))
      }
      else if(hands.contains(1))
        changeAceToEleven(hands.count(_ == 1), hands, hitRange) match {
          case Some(x) =>
            val score = x.sum
            val denominator = hitRatioByLengthCalculation(x.length -2,parentDeck.length)
            Seq(Rations(score,denominator,count))
          case None =>
            for{
              t <- deck
              l <- loopHit(hands :+ t,deck.diff(Seq(t)),parentDeck,hitRange,count)
            } yield l
        }
      else
        for{
          t <- deck
          l <- loopHit(hands :+ t,deck.diff(Seq(t)),parentDeck,hitRange,count)
        } yield l
    }

  val futureLoopHit: (Hands, Deck, Deck, Int => Boolean) => Future[Seq[Rations]] =
    (hands: Hands,
     deck: Deck,
     parentDeck: Deck,
     isInRange: Int => Boolean) =>
      Future(loopHit(hands,deck,parentDeck,isInRange,0))

  /*
  山札の枚数分非同期実行
   */
  val asynchronousCalculationSeparated:
    (Hands,Deck,Deck,Int => Boolean,(Hands, Deck, Deck, Int => Boolean) => Future[Seq[Rations]])
      => Future[Seq[Seq[Rations]]] =
    (hands: Hands,
     deck: Deck,
     parentDeck: Deck,
     isInRange: Int => Boolean,
     futureFunction: (Hands, Deck, Deck, Int => Boolean) => Future[Seq[Rations]])
    => {
      val futures: Seq[Future[Seq[Rations]]] =
        for{
          t <- deck
        } yield futureFunction(hands :+ t, deck.diff(Seq(t)),parentDeck,isInRange)
      Future.sequence(futures)
    }
}
