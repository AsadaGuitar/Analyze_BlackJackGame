package blackJack

import blackJack.BasicStrategy._
import blackJack.DetailedStrategy.{OriginalData, getRefinedAction}

/*
name  BlackJackUtility
func  クライアントサイドのユーティリティオブジェクト
 */
object BlackJackUtility {

  /*
  初期情報を受取り最善手を返却
   */
  def calculateHandsToAction(originalData: OriginalData): (Action,CalculationType) = {

    val playerHands = originalData.playerHands
    val dealerHand = originalData.dealerHands

    if (17 <= playerHands.sum) Stand
    try {
      val action = getRefinedAction(originalData)
      (action,Refined)
    }
    catch {
      case _: Exception =>
        val action = getSimplifiedAction(playerHands, dealerHand)
        (action,Simplified)
    }
  }
}
