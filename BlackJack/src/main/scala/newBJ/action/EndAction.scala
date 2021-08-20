package newBJ.action

import newBJ.{End, Start, SystemCommand}

trait EndAction extends ActionIO {

  def getAbleDealerHand: Option[String] =
    readDealerHand() match {
      case "q" => None
      case x => Some(x)
    }

//  @tailrec
//  private def dealerHit(dealerHands: Hand): Hand =
//    getAbleDealerHand match {
//      case None => dealerHands
//      case Some(x) =>
//        try {
//          dealerHands + x.toInt
//        }
//        catch {
//          case _: Exception => dealerHit(dealerHands)
//        }
//    }

//  def dealerHit() = dealerHit(new Hand())

  def getAbleSystemCommand: Option[SystemCommand] =
    readCommand().toUpperCase match {
      case "CONTINUE" => Some(Start)
      case "FINISH" => Some(End)
      case _ => None
    }

  def systemCommand(): SystemCommand =
    getAbleSystemCommand match {
      case None => systemCommand()
      case Some(x) => x
    }
}
