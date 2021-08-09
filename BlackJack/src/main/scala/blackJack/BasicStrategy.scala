package blackJack

/*
name  BasicStrategy
func  ベーシックストラテジーの評価集合
 */
object BasicStrategy {

  private def hardHand(playerScore: Int, dealerScore: Int): Action = playerScore match {
    case score if score <= 8 => Hit
    case score if 17 <= score => Stand
    case score if (13 to 16).contains(score) =>
      dealerScore match {
        case score if (2 to 6).contains(score) => Stand
        case _ => Hit
      }
    case 9 =>
      dealerScore match {
        case score if (3 to 6).contains(score) => DoubleDown
        case _ => Hit
      }
    case 10 =>
      dealerScore match {
        case score if score == 10 || score == 1 => Hit
        case _ => DoubleDown
      }
    case 11 =>
      dealerScore match {
        case 1 => Hit
        case _ => DoubleDown
      }
    case 12 =>
      dealerScore match {
        case score if (4 to 6).contains(score) => Stand
        case _ => Hit
      }
  }

  private def softHand(scoreNotAce: Int, dealerScore: Int): Action = scoreNotAce match {
    case score if score == 4 || score == 5 =>
      dealerScore match {
        case score if (4 to 6).contains(score) => Stand
        case _ => Hit
      }
    case score if score == 8 || score == 9 => Stand
    case 2 =>
      dealerScore match {
        case score if score == 5 || score == 6 => DoubleDown
        case _ => Hit
      }
    case 3 =>
      dealerScore match {
        case 4 => Stand
        case score if score == 5 || score == 6 => DoubleDown
        case _ => Hit
      }
    case 6 =>
      dealerScore match {
        case score if (3 to 6).contains(score) => Stand
        case _ => Hit
      }
    case 7 =>
      dealerScore match {
        case score if (3 to 6).contains(score) => DoubleDown
        case score if Seq(2,7,8).contains(score) => Stand
        case _ => Hit
      }
  }

  def getSimplifiedAction(playerHands: Hands, dealerHand: Int): Action = playerHands match {
    case hands if hands.contains(1) =>
      val score = hands.diff(Seq(1)).sum
      val action: Action = softHand(score, dealerHand)
      action
    case _ =>
      val score = playerHands.sum
      val action: Action = hardHand(score, dealerHand)
      action
  }
}
