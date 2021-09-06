package com.analysis.common.io

import cats.effect.IO

object MonadicIO {

  /*
  メソッド名     putStrLn
  機能          「文字列を標準出力する動作」を返却
  引数          str: String     　出力する文字列
  戻値          IO[Unit]        　「出力する動作」を返却
  */
  def putStrLn(str: String): IO[Unit] = IO(println(str))

  /*
  メソッド名     readLn
  機能          「文字列を標準入力する動作」を返却
  戻値          IO[String]        「読取る動作」を返却
  */
  def readLn(): IO[String] = IO(io.StdIn.readLine())

  /*
  メソッド名     readInt
  機能          「文字列を読込み、数値変換した結果を返却する動作」を返却
  戻値          IO[Unit]           「メッセージを出力し、数値を読取るまで入力を繰り返す動作」
  */
  def readInt(): IO[Either[NumberFormatException, Int]] = {
    readLn().map(line => line match {
      case ln if (1 to 10).map(_.toString).contains(ln) => Right(ln.toInt)
      case _ => Left(new NumberFormatException)
    })
  }

  /*
  メソッド名     getCurrentTimrMillis
  機能          「現在時刻をミリ秒で読込動作」を返却
  戻値          IO[Long]              「現在時刻をミリ秒で読込動作」
  */
  def getCurrentTimeMillis(): IO[Long] = IO(System.currentTimeMillis())
}
