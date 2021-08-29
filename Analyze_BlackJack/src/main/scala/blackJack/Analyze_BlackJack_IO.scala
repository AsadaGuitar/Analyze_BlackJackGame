package blackJack

object Analyze_BlackJack_IO {

  /*
  メソッド名   readInt
  機能        数値の標準入力
  戻値        入力値
  */
  def readInt(): Int = {
    //文字列の標準入力
    val input = io.StdIn.readLine()

    //数値変換で例外が発生した場合、再度実行
    try {
      input.toInt
    }
    catch {
      case e: java.lang.NumberFormatException =>
        println("[入力値不正] : 数値を入力してください。")
        readInt()
    }
  }

  /*
  メソッド名     readHand
  機能          手札の標準入力
  戻値          Hand      リスト(Hand)に変換された入力値
  */
  def readHand(): Hand = {
    //数値を取得
    val num = readInt()

    //入力値が、使用するトランプの値に含まれていない場合再度実行
    if ((1 to 10).contains(num)) Seq(num)
    else {
      println("[入力値不正] : 1から10の数値を入力してください。")
      readHand()
    }
  }

  /*
  メソッド名     readAction
  機能          アクションの標準入力
  戻値          Action      入力されたアクション
  */
  def readAction(): Action = {
    //文字列の標準入力
    val input = io.StdIn.readLine()

    //合致するアクションを返却
    input.toUpperCase match {
      case i if i.equals(Hit.toString.toUpperCase) => Hit
      case i if i.equals(DoubleDown.toString.toUpperCase) => DoubleDown
      case i if i.equals(Stand.toString.toUpperCase) => Stand
      //合致しない場合、再度実行
      case _ =>
        println("[入力値不正] : \"Hit\",\"DoubleDown\",\"Stand\"のいずれかを入力してください。")
        readAction()
    }
  }

  /*
  メソッド名     readDealerHitTramps
  機能          アクション終了後にディーラがHitしたトランプの標準入力
  戻値          Seq[Tramp]    ディーラがHitしたトランプ
  */
  def readDealerHitTramps(): Seq[Tramp] = {
    //"q"が入力されるまでトランプを取得
    def addTramp(tramps: Seq[Tramp]): Seq[Tramp] = {
      //文字列の標準入力
      val input = io.StdIn.readLine()
      input match {
        case "q" => tramps
        case in if (1 to 10).map(_.toString).contains(in) => addTramp(tramps :+ in.toInt)
        //1~10に含まれておらず"q"でもない場合、再度実行
        case _ =>
          println("[入力値不正] : 1から10の数値を入力してください。\n終了する場合は\"q\"を入力してください。")
          addTramp(tramps)
      }
    }
    addTramp(Seq(): Seq[Tramp])
  }

  /*
  メソッド名     readSystemCommand
  機能          システムコマンドの標準入力
  戻値          SystemCommand   入力されたシステムコマンド
  */
  def readSystemCommand(): SystemCommand = {
    //文字列の標準入力
    val input = io.StdIn.readLine()
    //合致するシステムコマンドを返却
    input.toUpperCase match {
      case i if i.equals(Continue.toString.toUpperCase) => Continue
      case i if i.equals(Init.toString.toUpperCase) => Init
      case i if i.equals(Finish.toString.toUpperCase) => Finish
      //合致しない場合、再度実行
      case _ =>
        println("[入力値不正] : \"Continue\",\"Init\",\"Finish\"のいずれかを入力してください。")
        readSystemCommand()
    }
  }

}
