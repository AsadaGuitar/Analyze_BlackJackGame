package newBJ.probability

import newBJ.{Deck, Hand, productOfList}

object Statistical {

  def calculateProbability(hand: Hand,deck: Deck): Rational = {
    val r: List[Int] = (((deck.length - hand.length) + 1) to deck.length).toList
    new Rational(1, productOfList(r).toLong)
  }

  def entrySet(hands: Seq[Hand], deck: Deck): Statistical ={

    def createMap(nodes: (Int,Rational), map: Map[Int,Rational]): Map[Int,Rational] =
      nodes._1 match {
        case score if map.contains(score) =>
          val newProbability = nodes._2
          val oldProbability = map(score)
          map.updated(score, newProbability + oldProbability)
        case _ => map.updated(nodes._1, nodes._2)
      }

    val probabilityNodes: Seq[(Int, Rational)] =
      for {h <- hands} yield (h.sum, calculateProbability(h,deck))

    val probabilityMap = probabilityNodes.foldLeft(Map(): Map[Int,Rational])((acc,x) => createMap(x,acc))
    new Statistical(probabilityMap)
  }
}

class Statistical (probabilityMap: Map[Int,Rational]){

  private val _map = probabilityMap
  def map = _map

  def filterSum(filter: Int => Boolean) =
    map.filter(x => filter(x._1)).values.reduceLeft((acc, x) => acc + x)
}
