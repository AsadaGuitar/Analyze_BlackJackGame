package newBJ.calculation.probability.Statistics

import newBJ.calculation.probability.Rational

object Statistical {

  def productOfList(ints: List[Int]): BigInt = {
    def loop(ints: List[Int]): BigInt = ints match {
      case Nil => 1
      case head :: tail => head * loop(tail)
    }
    if (ints.isEmpty) 0
    else loop(ints)
  }
}

abstract class Statistical[K](probsMap: Map[K,Rational]) {

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
