package com.analysis.blackJack.io

import cats.effect.IO.fromEither
import cats.effect.*

import scala.util.Try
import scala.util.matching.Regex

import com.analysis.blackJack._
import com.analysis.common.AnalysisIO._

object BlackJackIO {

  /*
  メソッド名     readAction
  機能          「メッセージを出力し、アクションを標準入力で読取る動作」を返却
  引数          msg: String       出力するメッセージ
  　　          errMsg: String    例外メッセージ
  戻値          IO[Action]        「メッセージを出力し、アクションを標準入力で読取る動作」
  */
  def readAction(): IO[Action] =
    //アクション読込
    readLn().flatMap(x => x.toUpperCase match {
      case "HIT" => IO(Hit) 
      case "DOUBLEDOWN" => IO(DoubleDown) 
      case "STAND" => IO(Stand) 
      //不正な値を読込んだ場合、例外メッセージを出力し再度実行
      case _ => readAction(msg, errMsg)
  })
  
  /*
  メソッド名     readSystemCommand
  機能          「メッセージを出力し、システムコマンドを標準入力で読取る動作」を返却
  引数          msg: String      出力メッセージ
  　　          errMsg: String   例外メッセージ
  戻値          SystemCommand    「メッセージを出力し、システムコマンドを標準入力で読取る動作」
  */
  def readSystemCommand(): IO[SystemCommand] =
    //文字列読込
    readLn().flatMap(line => line.toUpperCase match {
      case "CONTINUE" => IO(Continue)
      case "INIT" => IO(Init)
      case "FINISH" => IO(Finish)
      //不正な文字列を受取った場合、例外メッセージを出力し再度実行
      case _ => readSystemCommand()
    })
}
