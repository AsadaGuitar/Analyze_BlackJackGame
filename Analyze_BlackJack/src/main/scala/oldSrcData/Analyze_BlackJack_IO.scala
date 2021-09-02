package blackJack

import cats.effect.IO.fromEither
import cats.effect.{ContextShift, IO, Sync, SyncIO}
import cats.effect.*

import scala.util.Try
import scala.util.matching.Regex

object Analyze_BlackJack_IO {

  /*
  メソッド名     putStrLn
  機能          「文字列を標準出力する動作」を返却
  引数          str: String     出力する文字列
  戻値          IO[Unit]        「出力する動作」を返却
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
  機能          「メッセージを出力し、数値を読取るまで入力を繰り返す動作」を返却
  引数          msg: String                 出力する文字列
  　　          errMsg: String              例外メッセージ
  　　          filter: String => Boolean   読取る数値のフィルター
  戻値          IO[Unit]                    「メッセージを出力し、数値を読取るまで入力を繰り返す動作」
  */
  def readInt(msg: String, errMsg: String, filter: String => Boolean): IO[Int] = {
    //メッセージ出力
    putStrLn(msg) *>
    //数値読込
    readLn().flatMap(ln => 
      //指定した数値に含まれていた場合返却
      if (filter(ln)) {
        IO(ln.toInt)
      } 
      //指定外の数値の場合、例外メッセージを出力し再度実行
      else{
        putStrLn(errMsg) *> readInt(msg, errMsg, filter)
      })
  }
  
  /*
  メソッド名     readAction
  機能          「メッセージを出力し、アクションを標準入力で読取る動作」を返却
  引数          msg: String       出力するメッセージ
  　　          errMsg: String    例外メッセージ
  戻値          IO[Action]        「メッセージを出力し、アクションを標準入力で読取る動作」
  */
  def readAction(msg: String, errMsg: String): IO[Action] = 
    //メッセージ出力
    putStrLn(msg) *>
    //アクション読込
    readLn().flatMap(x => x.toUpperCase match {
      case "HIT" => IO(Hit) 
      case "DOUBLEDOWN" => IO(DoubleDown) 
      case "STAND" => IO(Stand) 
      //不正な値を読込んだ場合、例外メッセージを出力し再度実行
      case _ => putStrLn(errMsg) *> readAction(msg, errMsg)
  })
  
  /*
  メソッド名     readHandRepetition
  機能          「指定の文字列を受取るまで指定の数値を読込動作」を返却
  引数          msg: String                   出力メッセージ
  　　          errMsg: String                例外メッセージ
  　　          period: String                読込終了文字列
  　　          filter: String => Boolean     読込文字列の指定
  　　          tramps: Seq[Tramp]            読込んだ文字列
  戻値          IO[Seq[Tramp]]                「指定の文字列を受取るまで指定の数値を読込動作」
  */
  private def readHandRepetition(msg: String, 
                                 errMsg: String, 
                                 period: String,
                                 filter: String => Boolean,
                                 tramps: Seq[Tramp]): IO[Seq[Tramp]] = {
    //メッセージ出力
    putStrLn(msg) *> 
    //文字列読込
    readLn().flatMap(line => line match {
      //読込を終了する文字列を受取った場合、リストを返却
      case ln if (ln == period) => IO(tramps)
      //指定の文字列を受取った場合、リストに追加し再度実行
      case ln if(filter(ln)) => readHandRepetition(msg, errMsg, period, filter, tramps :+ ln.toInt)
      //不正な文字列を受取った場合、例外メッセージを出力し再度実行
      case _ => putStrLn(errMsg) *> readHandRepetition(msg, errMsg, period, filter, tramps)
    })
  }

  /*
  メソッド名     readHandRepetition
  機能          「指定の文字列を受取るまで指定の数値を読込動作」を返却
  引数          msg: String                   出力メッセージ
  　　          errMsg: String                例外メッセージ
  　　          period: String                読込終了文字列
  　　          filter: String => Boolean     読込文字列の指定
  戻値          IO[Seq[Tramp]]                「指定の文字列を受取るまで指定の数値を読込動作」
  */
  def readHandRepetition(msg: String, errMsg: String, period: String, filter: String => Boolean): IO[Seq[Tramp]] = 
    readHandRepetition(msg, errMsg, period, filter, Nil)
  
  /*
  メソッド名     readSystemCommand
  機能          「メッセージを出力し、システムコマンドを標準入力で読取る動作」を返却
  引数          msg: String      出力メッセージ
  　　          errMsg: String   例外メッセージ
  戻値          SystemCommand    「メッセージを出力し、システムコマンドを標準入力で読取る動作」
  */
  def readSystemCommand(msg: String, errMsg: String): IO[SystemCommand] =
    //メッセージ出力
    putStrLn(msg) *> 
    //文字列読込
    readLn().flatMap(line => line.toUpperCase match {
      case "CONTINUE" => IO(Continue)
      case "INIT" => IO(Init)
      case "FINISH" => IO(Finish)
      //不正な文字列を受取った場合、例外メッセージを出力し再度実行
      case _ => putStrLn(errMsg) *> readSystemCommand(msg, errMsg)
    })
}
