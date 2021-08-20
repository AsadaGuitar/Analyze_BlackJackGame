

object Utility extends App {

  def fact(x: Int): a = x match {
    case 1 => new Aabs
  }

  val a = fact(1)

  a.f()

}

trait a {
  def f(): Unit
}

abstract class abs {

  val answer = "YES"

  def x(e: Int) = e + 1
  def y(e: Int): Int
}

class Aabs extends abs with a {
  override def y(e: Int): Int = e *10

  override def f(): Unit = println(x(10) + y(10))
}

class Babs extends abs with a{
  override def y(e: Int) = e * 100

  override def f(): Unit = println(x(10) + y(10))
}

