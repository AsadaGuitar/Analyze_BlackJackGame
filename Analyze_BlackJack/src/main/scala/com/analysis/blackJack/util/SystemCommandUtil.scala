package com.analysis.blackJack.util

/*
オブジェクト名     SystemCommandUtil
機能             システムコマンド、及びユーティリティを提供
*/
object SystemCommandUtil {
  
  /*
  クラス名        SystemCommand
  機能           システムコマンドの抽象クラス
   */
  sealed abstract class SystemCommand
  case object Continue extends SystemCommand
  case object Init extends SystemCommand
  case object Finish extends SystemCommand

  //システムコマンドのExceptionクラス
  final class SystemCommandFormatException(errMsg: String = "SystemCommandに変換出来ません。") extends Exception(errMsg)

  //システムコマンドへの変換メソッドを提供
  trait SystemCommandFormat {
    def toSystemCommand: Either[SystemCommandFormatException, SystemCommand]
  }

  /*
  暗黙クラス名　　　　StringSystemCommandFormat
  機能        　　　String型のオブジェクトにシステムコマンド変換メソッドを提供
  */
  implicit class StringSystemCommandFormat(str: String) extends SystemCommandFormat {
    def toSystemCommand: Either[SystemCommandFormatException, SystemCommand] = str.toUpperCase match {
      case "CONTINUE" => Right(Continue)
      case "INIT"     => Right(Init)
      case "FINISH"   => Right(Finish)
      case _          => Left(new SystemCommandFormatException())
    }
  }
  
  /*
  メソッド名       toSystemCommand
  機能            Stringを引数に受取りSystemCommandに変換
  引数            str: String                                              SystemCommandに変換するStringオブジェクト
  戻値            Either[SystemCommnandFormatException, SystemCommand]     SystemCommandに変換したオブジェクト
  */
  def toSystemCommand(str: String): Either[SystemCommandFormatException, SystemCommand] = {
    str.toSystemCommand
  }
}
