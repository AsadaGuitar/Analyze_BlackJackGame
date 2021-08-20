package newBJ.calculation.probability

import scala.annotation.tailrec

class Rational(n: Long, d: Long){
  require(d != 0)

  private val g = gcd(n.abs, d.abs)
  val num = n / g
  val denom = d / g

  def this(num: Int) = this(num, 1)

  override def toString: String = s"$num/$denom"

  @tailrec
  private def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

  def get() = num / denom.toDouble

  def +(that: Rational): Rational =
    new Rational(num * that.denom + that.num * denom, denom * that.denom)

  def +(n: Int): Rational =
    new Rational(num + n * denom, denom)

  def -(that: Rational): Rational =
    new Rational(num * that.denom - that.num * denom, denom * that.denom)

  def -(n: Int): Rational =
    new Rational(n - n * denom, denom)

  def *(that: Rational): Rational =
    new Rational(num * that.num, denom * that.denom)

  def *(n: Int): Rational =
    new Rational(num * (n * denom), denom)


  def < (that: Rational): Boolean = num * that.denom < that.num * denom
  def max(that: Rational): Rational = if (this < that) that else this

}