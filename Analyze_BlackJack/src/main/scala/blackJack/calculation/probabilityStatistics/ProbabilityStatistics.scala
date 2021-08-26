package blackJack.calculation.probabilityStatistics

import blackJack.calculation.Rational

object ProbabilityStatistics {

  def addMap[K](probs: (K,Rational), map: Map[K,Rational]): Map[K,Rational] =
    probs._1 match {
      case score if map.contains(score) =>
        val newProb = probs._2
        val oldProb = map(score)
        map.updated(score, newProb + oldProb)
      case _ => map.updated(probs._1, probs._2)
    }
}

class ProbabilityStatistics[K](probsMap: Map[K,Rational]) {
  import ProbabilityStatistics._

  private val _map: Map[K,Rational] = probsMap
  def map = _map

  def this() = this(Map())
  def this(that: (K,Rational)) = this(Map(that))

  def ?=(filter: K => Boolean) = {
    val rationals = map.filter(x => filter(x._1)).values
    if (rationals.isEmpty) None
    else Some(rationals.reduceLeft((acc, x) => acc + x))
  }

  def ++(that: ProbabilityStatistics[K]) =
    new ProbabilityStatistics[K](
      that.map.foldLeft(this.map)((acc,x) => addMap(x,acc))
    )

  def :+ (that: (K, Rational)): ProbabilityStatistics[K] =
    new ProbabilityStatistics[K](addMap(that,map))
}