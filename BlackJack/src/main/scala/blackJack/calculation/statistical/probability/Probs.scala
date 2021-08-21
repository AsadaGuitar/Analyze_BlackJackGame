package blackjack.calculation.statistical.probability

abstract class Probs[K](probsMap: Map[K,Rational]) {

  private val _map = probsMap
  def map = _map

  implicit class FilterMap(map : Map[K,Rational]) {

    def ?=(filter: K => Boolean) = {
      val rationals = map.filter(x => filter(x._1)).values
      if (rationals.isEmpty) None
      else Some(rationals.reduceLeft((acc, x) => acc + x))
    }
  }
}
