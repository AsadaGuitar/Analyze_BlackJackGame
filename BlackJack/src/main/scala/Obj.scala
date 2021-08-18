import newBJ._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Obj extends App {


  val a = Seq(1,6,2)

  a.foreach(println)

  val user = new Player(a)
  val cUser = user.exchangeAce((17 to 21).contains)

  println(cUser.hand)

}
