package blackJack

import blackJack.BasicStrategy._
import blackJack.DetailedStrategy.{OriginalData, getRefinedAction}

object BlackJackUtility {

  abstract class CalculationType
  case object Simplified extends CalculationType
  case object Refined extends CalculationType

  /*
  name    initDeck
  func    山札の数と使用するトランプを受取り、個数分のトランプをリストに格納し、返却
  args    deckNum: Int        使用する山札の数
          tramps: Seq[Int]    使用するトランプ
  return  Seq[Int]            使用する個数分を格納したリスト
   */
  def initDeck(deckNum: Int, tramps: Seq[Int]): Deck = {
    def addTramps(num: Int, ts: Seq[Int]): Seq[Int] = {
      if (num <= 1) ts
      else ts ++ addTramps(num - 1, ts)
    }
    if (deckNum < 1) Nil
    else addTramps(deckNum * 4, tramps)
  }

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

  def deleteHandsAtDeck(deck: Deck,hand: Int): Deck ={
    deck.diff(Seq(hand))
  }
}
