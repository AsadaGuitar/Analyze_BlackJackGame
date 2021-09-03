package com.analysis.common.calculation

import scala.annotation.tailrec

/*
クラス名            Rational
機能            　　分数クラス
*/
class Rational(n: BigInt, d: BigInt) {
  
  //第二引数(分母)が0の場合例外を発生
  require(d != 0)

  //第一引数と第二引数の最小約数を取得
  private val g = gcd(n.abs, d.abs)
  val num = n / g
  val denom = d / g

  /*
  メソッド名         this
  機能              コンストラクタのオーバーロード
  */
  def this(num: Int) = this(num, 1)

  /*
  メソッド名         toString
  機能              分数表記の文字列に変換
  戻値              String                                      第一引数、第二引数を分数表記に変換した文字列
  */
  override def toString: String = s"$num/$denom"

  /*
  メソッド名         gcd
  機能              ユークリッドの互除法により最小約数を算出
  引数              a: BigInt                                   分子    
                　　b: BigInt                                   分母
  戻値              BigInt                                      最小約数
  */
  @tailrec
  private def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a else gcd(b, a % b)

  /*
  メソッド名         get
  機能              設定値の分数を浮動小数に変換
  戻値              BigDecimal                                  設定された分数を浮動小数に変換した値
  */
  def get() = BigDecimal(num) / BigDecimal(denom)
  
  /*
  メソッド名         +
  機能              受取ったRationalを加算した結果を返却
  引数              that: Rational                              加算するRational
  戻値              Rational                                    受取ったRationalとの加算結果
  
  */
  def +(that: Rational): Rational =
    new Rational(num * that.denom + that.num * denom, denom * that.denom)

  /*
  メソッド名         +
  機能              受取った整数値を加算した結果を返却
  引数              that: Int                                    加算する整数値
  戻値              Rational                                     受取った整数値との加算結果
  */
  def +(n: Int): Rational = new Rational(num + n * denom, denom)

  /*
  メソッド名         -
  機能              受取ったRationalを減算した結果を返却
  引数              that: Rational                               減算するRational
  戻値              Rational                                     受取ったRationalとの減算結果
  */
  def -(that: Rational): Rational =
    new Rational(num * that.denom - that.num * denom, denom * that.denom)

  /*
  メソッド名         -
  機能              受取った整数値を減算した結果を返却
  引数              that: Int                               　　  減算する整数値
  戻値              Int　                                         受取った整数値との減算結果
  */
  def -(n: Int): Rational = new Rational(n - n * denom, denom)

  /*
  メソッド名         *
  機能              受取ったRationalを乗算した結果を返却
  引数              Rational                                      乗算するRational
  戻値              Rational                                      受取ったRationalとの乗算結果
  */
  def *(that: Rational): Rational =
    new Rational(num * that.num, denom * that.denom)

  /*
  メソッド名         *
  機能              受取った整数値を乗算した結果を返却
  引数              Int　　　                                     乗算するRational
  戻値              Int     　　                                  受取ったRationalとの乗算結果
  */
  def *(n: Int): Rational =
    new Rational(num * (n * denom), denom)

  /*
  メソッド名         <
  機能              受取ったRationalとの比較結果
  引数              that: Rational                                  比較するRational
  戻値              Boolean                                         引数よりも値が小さい場合trueを返却
  */
  def <(that: Rational): Boolean = num * that.denom < that.num * denom

  /*
  メソッド名         <
  機能              受取った浮動小数との比較結果
  引数              that: Double                           　　     比較するDouble
  戻値              Boolean                                         引数よりも値が小さい場合trueを返却
  */
  def <(that: Double): Boolean = get() < that

  /*
  メソッド名         >
  機能              受取ったRationalとの比較結果
  引数              that: Rational                                  比較するRational
  戻値              Boolean                                         引数よりも値が大きい場合trueを返却
  */
  def >(that: Rational): Boolean = num * that.denom > that.num * denom

  /*
  メソッド名         >
  機能              受取った浮動小数との比較結果
  引数              that: Double                           　　      比較するDouble
  戻値              Boolean                                          引数よりも値が大きい場合trueを返却
  */
  def >(that: Double): Boolean = get() > that
}
