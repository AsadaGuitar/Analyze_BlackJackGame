package com.analysis.blackJack.io

import cats.effect.IO.fromEither
import cats.effect.*

import scala.util.Try
import scala.util.matching.Regex

import com.analysis.blackJack._
import com.analysis.common.AnalysisIO._

object BlackJackIO {

  /*
  メソッド名       readAction
  機能            「アクションを標準入力で読取る動作」を返却
  戻値            IO[Either[Action]]                          「メッセージを出力し、アクションを標準入力で読取る動作」
  */
  def readAction(): IO[Either[Action]] =
    //アクション読込
    readLn().map(x => x.toUpperCase match {
      case "HIT" => Right(Hit) 
      case "DOUBLEDOWN" => Right(DoubleDown) 
      case "STAND" => Right(Stand) 
      //不正な値を読込んだ場合、Either.Leftを返却
      case _ => Left(throw new IllegalArgumentException)
  })
  
  /*
  メソッド名     readSystemCommand
  機能          「システムコマンドを標準入力で読取る動作」を返却
  戻値          IO[Either[SystemCommand]]                      「メッセージを出力し、システムコマンドを標準入力で読取る動作」
  */
  def readSystemCommand(): IO[Either[SystemCommand]] =
    //文字列読込
    readLn().map(line => line.toUpperCase match {
      case "CONTINUE" => Right(Continue)
      case "INIT" => Right(Init)
      case "FINISH" => Right(Finish)
      //不正な文字列を受取った場合、例外メッセージを出力し再度実行
      case _ => Left(throw new IllegalArgumentException)
    })
}
