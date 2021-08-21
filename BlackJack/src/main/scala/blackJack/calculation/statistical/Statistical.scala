package blackjack.calculation.statistical

import blackjack.calculation.statistical.probability.Rational


trait StatisticalCreater[T]{

  def create(): ProbsStatistical[T]
}

abstract class ProbsStatistical[T]{

  def toList: Seq[(T,Rational)]

  def toMap: Map[T,Rational]

}
