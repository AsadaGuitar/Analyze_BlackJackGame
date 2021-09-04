package com.analysis.blackJack.io

import cats.effect.IO.fromEither
import cats.effect.*

import scala.util.Try
import scala.util.matching.Regex

import com.analysis.blackJack._
import com.analysis.common.io.MonadicIO._

object BlackJackIO {
  
  
  def readTrumpRepetition(msg: String, errMsg: String): IO[Hand] ={
    putStrLn(msg) *>
      readInt().flatMap(x => x match {
        case Right(x) if (1 to 10).contains(x) => IO(Seq(x))
        case _ => putStrLn(errMsg) *> readTrumpRepetition(msg, errMsg)
      })
  }

  /*
  メソッド名       readAction
  機能            「アクションを標準入力で読取る動作」を返却
  戻値            IO[Either[Action]]                          「メッセージを出力し、アクションを標準入力で読取る動作」
  */
  private def readAction(): IO[Either[IllegalArgumentException, Action]] =
  //アクション読込
    readLn().map(x => x.toUpperCase match {
      case "HIT"        => Right(Hit)
      case "DOUBLEDOWN" => Right(DoubleDown)
      case "STAND"      => Right(Stand)
      //不正な値を読込んだ場合、Either.Leftを返却
      case _ => Left(throw new IllegalArgumentException)
    })
    
  def readActionRepetition(msg: String, errMsg: String): IO[Action] = {
    putStrLn(msg) *>
      readAction().flatMap(x => x match {
        case Right(a) => IO(a)
        case Left(_)  => putStrLn(errMsg) *> readActionRepetition(msg, errMsg)
      })
  }

  def readDealerHitTrumps(msg: String, errMsg: String, period: String): IO[Hand] = {
    def function(hand: Hand): IO[Hand] = {
      putStrLn(msg) *>
        //文字列読込
        readLn().flatMap(line => line match {
          //読込を終了する文字列を受取った場合、リストを返却
          case ln if (ln == period)                         => IO(hand)
          //指定の文字列を受取った場合、リストに追加し再度実行
          case ln if (1 to 10).map(_.toString).contains(ln) => function(hand :+ ln.toInt)
          //不正な文字列を受取った場合、例外メッセージを出力し再度実行
          case _ => putStrLn(errMsg) *> function(hand)
        })
    }
    function(Nil)
  }

  /*
  メソッド名     readSystemCommand
  機能          「システムコマンドを標準入力で読取る動作」を返却
  戻値          IO[Either[SystemCommand]]                      「メッセージを出力し、システムコマンドを標準入力で読取る動作」
  */
  private def readSystemCommand(): IO[Either[IllegalArgumentException, SystemCommand]] =
  //文字列読込
    readLn().map(line => line.toUpperCase match {
      case "CONTINUE" => Right(Continue)
      case "INIT"     => Right(Init)
      case "FINISH"   => Right(Finish)
      //不正な文字列を受取った場合、例外メッセージを出力し再度実行
      case _ => Left(throw new IllegalArgumentException)
    })

  def readSystemCommandRepetition(msg: String, errMsg: String): IO[SystemCommand] = {
    putStrLn(msg) *>
      readSystemCommand().flatMap(x => x match {
        case Right(a) => IO(a)
        case Left(_)  => putStrLn(errMsg) *> readSystemCommandRepetition(msg, errMsg)
      })
  }
}
