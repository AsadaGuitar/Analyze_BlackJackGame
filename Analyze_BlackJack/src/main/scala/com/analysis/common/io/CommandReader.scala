package com.analysis.common.io

object CommandReader {

  def readCommand[E <: Exception, A](period: String = "end")
                                    (exchange: String => Either[E, A]): Option[A] = {
    val line = scala.io.StdIn.readLine()

    if (line == period) None
    else exchange(line) match {
      case Right(ln) => Some(ln)
      case Left(e) =>
        println(e.getMessage)
        readCommand(period)(exchange)
    }
  }

  def readCommandList[E <: Exception, A](readingPeriod: String, systemPeriod: String = "end")
                                        (exchange: String => Either[E, A]): Option[List[A]] = {
    def readRepetition(readList: List[A]): Option[List[A]] = {
      val line = scala.io.StdIn.readLine()
      if (line.equals(systemPeriod)) {
        None
      }
      else {
        if (line == readingPeriod) Some(readList)
        else exchange(line) match {
          case Right(ln) => readRepetition(readList :+ ln)
          case Left(e) =>
            println(e.getMessage)
            readRepetition(readList)
        }
      }
    }
    readRepetition(Nil)
  }
}
