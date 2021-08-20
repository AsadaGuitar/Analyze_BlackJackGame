package newBJ.calculation.probability

trait ProbsListCreater[T] {

  def create(): Map[T, Rational]
}

class MaybeHitProbs extends ProbsListCreater[Int] {
  override def create() = Map()
}