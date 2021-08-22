package oldblackJack

import oldblackJack.BasicStrategy._
import oldblackJack.DetailedStrategy.{OriginalData, getRefinedAction}

/*
name  BlackJackUtility
func  クライアントサイドのユーティリティオブジェクト
 */
object BlackJackUtility {

  /*
  メソッド名     calculateHandsToAction
  機能          初期情報を受取り最善手を返却
  引数          originalData: OriginalData    初期情報
  返値          (Action,CalculationType)
               Action             計算後に入力されたアクション
               CalculationType    計算の種類
   */
  def calculateHandsToAction(originalData: OriginalData): (Action,CalculationType) = {

    val playerHands = originalData.playerHands
    val dealerHand = originalData.dealerHands

    try {
      //詳細計算を開始
      val action = getRefinedAction(originalData)
      (action,Refined)
    }
    catch {
      //詳細計算がタイムアウトした場合
      case _: concurrent.TimeoutException =>
        //簡略計算を開始
        val action = getSimplifiedAction(playerHands, dealerHand)
        (action,Simplified)
    }
  }
}
