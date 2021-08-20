package newBJ.action

trait ActionIO {

  def readDealerHand(): String ={
    println("ディーラの手札を入力してください。")
    io.StdIn.readLine()
  }

  def readCommand(): String ={
    println("コマンドを入力してください。")
    io.StdIn.readLine()
  }
}
