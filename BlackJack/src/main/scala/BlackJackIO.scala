import scala.annotation.tailrec

object BlackJackIO {

  abstract class Player
  case object User extends Player
  case object Dealer extends Player

  @tailrec
  def readDeckNum(): Int = {
    println("使用する山札の数(1~10)を入力してください。")
    val n = io.StdIn.readLine()
    if((1 to 10).map(_.toString) contains n) n.toInt
    else readDeckNum()
  }

  @tailrec
  def readHand(player: Player): Int ={
    println({
      if(player == User) "ユーザの手札を1枚ずつ教えてください。"
      else "ディーラの手札を教えてください。"
    })
    val n = io.StdIn.readLine()
    if((1 to 11).map(_.toString) contains n) n.toInt
    else readHand(player)
  }
}
