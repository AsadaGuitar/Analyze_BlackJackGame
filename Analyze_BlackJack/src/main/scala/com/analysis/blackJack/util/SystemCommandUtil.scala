package com.analysis.blackJack.util

object SystemCommandUtil {
  
  sealed abstract class SystemCommand
  case object Continue extends SystemCommand
  case object Init extends SystemCommand
  case object Finish extends SystemCommand

  final class SystemCommandFormatException(errMsg: String = "SystemCommandに変換出来ません。") extends Exception(errMsg)

  trait SystemCommandFormat {
    def toSystemCommand: Either[SystemCommandFormatException, SystemCommand]
  }

  implicit class StringSystemCommandFormat(str: String) extends SystemCommandFormat {
    def toSystemCommand: Either[SystemCommandFormatException, SystemCommand] = str.toUpperCase match {
      case "CONTINUE" => Right(Continue)
      case "INIT"     => Right(Init)
      case "FINISH"   => Right(Finish)
      case _          => Left(new SystemCommandFormatException())
    }
  }
  
  def toSystemCommand(str: String): Either[SystemCommandFormatException, SystemCommand] = {
    str.toSystemCommand
  }
}
