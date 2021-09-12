package com.analysis.common.io

/*
オブジェクト名     CommandReader
機能             標準入力機能を提供  
*/
object CommandReader {

  /*
  メソッド名     readCommand[E <: Exception, A]
  機能          システムを終了する文字列、もしくはオブジェクトに変換可能な文字列を受取るまで標準入力をループ
  引数          period  : String                     システムを終了する文字列
               exchange: String => Either[E,A]　　　　文字列をオブジェクトに変換する関数
  戻値          Option[A]                        　　 システム終了文字列の場合Noneを返却
  */
  def readCommand[E <: Exception, A](period: String = "end")
                                    (exchange: String => Either[E, A]): Option[A] = {
    //文字列読込
    val line = scala.io.StdIn.readLine()
    
    //文字列がシステムを終了する文字列の場合Noneを返却
    if (line == period) None
    //オブジェクトに変換して条件分岐
    else exchange(line) match {
      //オブジェクトに変換可能な場合、変換されたオブジェクトを返却
      case Right(ln) => Some(ln)
      //変換不可能ば場合、エラーメッセージを出力し、再度実行
      case Left(e) =>
        println(e.getMessage)
        readCommand(period)(exchange)
    }
  }

  /*
  メソッド名　　　　readCommandList[E <: Exception, A]
  機能　　　　　　　システムを終了する文字列を受取った場合、Noneを返却
  　　　　　　　　　読込みを終了する文字列を受取った場合、文字列を変換したオブジェクトが格納されているリストを返却
  引数　　　　　　　readingPeriod: String                     読込みを終了する文字列
  　　　　　　　　　period       : String　　　　　　　　　　　　　システムを終了する文字列
  　　　　　　　　　exchange     : String => Either[E,A]　　　　文字列をオブジェクトに変換する関数
  戻値　　　　　　　Option[List[A]]                        　　システム終了文字列の場合Noneを返却
  */
  def readCommandList[E <: Exception, A](readingPeriod: String, systemPeriod: String = "end")
                                        (exchange: String => Either[E, A]): Option[List[A]] = {
    def readRepetition(readList: List[A]): Option[List[A]] = {
      //文字列読込
      val line = scala.io.StdIn.readLine()
      
      line match {
        //文字列がシステムを終了する文字列だった場合、Noneを返却
        case ln if ln == systemPeriod  => None
        //文字列が読込を終了する文字列だった場合、読込リストを返却
        case ln if ln == readingPeriod => Some(readList)
        //文字列をオブジェクトに変換して条件分岐
        case ln => exchange(ln) match {
          //オブジェクトに変換可能な場合リストに加えて再度実行
          case Right(a) => readRepetition(readList :+ a)
          //オブジェクトに変換不可能な場合、エラーメッセージを出力し再度実行
          case Left(e) =>
            println(e.getMessage)
            readRepetition(readList)
        }
      }
    }
    readRepetition(Nil)
  }
}
