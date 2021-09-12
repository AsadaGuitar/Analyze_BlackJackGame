package com.analysis.blackJack.util

/*
オブジェクト名       ActionUtil
機能               Actionオブジェクトのユーティリティオブジェクト
*/
object ActionUtil {

  /*
  クラス名      Action
  機能        　ユーザーアクションの抽象クラス
  */
  sealed abstract class Action
  case object Hit extends Action
  case object Stand extends Action
  case object DoubleDown extends Action

  //オブジェクトをActionに変換する際のExceptionクラス
  final class ActionFormatException(errMsg: String = "Actionに変換出来ません。") extends Exception(errMsg)

  //Actionオブジェクトに変換する機能を提供
  trait ActionFormat {
    def toAction: Either[ActionFormatException, Action]
  }

  /*
  暗黙クラス名　　　　StringActionFormat
  機能        　　　String型のオブジェクトにアクションオブジェクト変換メソッドを提供
  */
  implicit class StringActionFormat(str: String) extends ActionFormat {
    def toAction: Either[ActionFormatException, Action] = str.toUpperCase match {
      case "HIT"        => Right(Hit)
      case "STAND"      => Right(Stand)
      case "DOUBLEDOWN" => Right(DoubleDown)
      case _            => Left(new ActionFormatException())
    }
  }

  /*
  メソッド名       toAction
  機能            Stringを引数に受取りActionに変換
  引数            str: String                                              Actionに変換するStringオブジェクト
  戻値            Either[SystemCommnandFormatException, Action]            Actionに変換したオブジェクト
  */
  def toAction(str: String): Either[ActionFormatException, Action] = {
    str.toAction
  }
}
