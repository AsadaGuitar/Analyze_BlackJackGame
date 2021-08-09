package blackJack

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/*
name  DetailedCalculation
func  ディーラが引く可能性のある組み合わせを全件取得
 */
object DetailedCalculation {

  case class Rations(tramp: Int, probability: BigDecimal, calculatedCount: Long)

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
    (Hands, Deck, Deck, Int => Boolean,
      (Hands, Deck, Deck, Int => Boolean) => Future[Seq[Rations]])
      => Future[Seq[Seq[Rations]]] =
    (hands: Hands,
     deck: Deck,
     parentDeck: Deck,
     isInRange: Int => Boolean,
     futureFunction: (Hands, Deck, Deck, Int => Boolean) => Future[Seq[Rations]]) => {

      val futures: Seq[Future[Seq[Rations]]] =
        for{
          t <- deck
        } yield futureFunction(hands :+ t, deck.diff(Seq(t)),parentDeck,isInRange)
      Future.sequence(futures)
    }
}
