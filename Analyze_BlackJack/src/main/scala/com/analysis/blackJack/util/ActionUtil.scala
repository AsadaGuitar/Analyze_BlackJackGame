package com.analysis.blackJack.util


object ActionUtil {

  sealed abstract class Action
  case object Hit extends Action
  case object Stand extends Action
  case object DoubleDown extends Action

  final class ActionFormatException(errMsg: String = "Actionに変換出来ません。") extends Exception(errMsg)

  trait ActionFormat {
    def toAction: Either[ActionFormatException, Action]
  }

  object ActionFormat {
    implicit class StringActionFormat(str: String) extends ActionFormat {
      def toAction: Either[ActionFormatException, Action] = str.toUpperCase match {
        case "HIT"        => Right(Hit)
        case "STAND"      => Right(Stand)
        case "DOUBLEDOWN" => Right(DoubleDown)
        case _            => Left(new ActionFormatException())
      }
    }
  }

  def toAction(str: String): Either[ActionFormatException, Action] = {
    import ActionFormat._
    str.toAction
  }
}
