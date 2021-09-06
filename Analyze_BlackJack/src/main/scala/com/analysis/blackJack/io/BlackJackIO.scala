package com.analysis.blackJack.io

import cats.effect.IO.fromEither
import cats.effect.*

import scala.util.Try
import scala.util.matching.Regex

import com.analysis.blackJack._
import com.analysis.common.io.MonadicIO._
import com.analysis.blackJack.util.SystemCommandUtil._
import com.analysis.blackJack.util.ActionUtil._
import com.analysis.blackJack.util.HandUtil._

object BlackJackIO {

  def readCommandRepetition[E <: Exception, A](exchange: String => Either[E,A]): IO[A] = {
    readLn().flatMap(ln => exchange(ln) match {
      case Right(r) => IO(r)
      case Left(e)  => putStrLn(e.getMessage) *> readCommandRepetition(exchange)
    })
  }

  def readCommandUtilPeriod[E <: Exception, A](period: String, exchange: String => Either[E,A]): IO[List[A]] ={
    def repeatReading(readList: List[A]): IO[List[A]] = {
      //文字列読込
      readLn().flatMap(ln =>
        if (ln.equals(period)) IO(readList)
        else exchange(ln) match {
          case Right(r) => repeatReading(readList :+ r)
          case Left(e)  => putStrLn(e.getMessage) *> repeatReading(readList)
        })
    }
    repeatReading(Nil)
  }
}
