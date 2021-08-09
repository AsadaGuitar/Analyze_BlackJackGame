import blackJack.BlackJackIO.{Dealer, Player, User, UserHit, readAction, readDealerHands, readDeckNum, readGameCommand, readHand}
import blackJack.BlackJackUtility.calculateHandsToAction
import blackJack.DetailedStrategy.OriginalData
import blackJack._

import scala.annotation.tailrec

object MainUtility {

  case class FirstFlowResult(playerHands: Hands,
                             dealerHand: Int,
                             deck: Deck,
                             action: Action)
  case class MainFlowResult(gameCommand: GameCommand,
                            deck: Deck)
  case class ActionFlowResult(gameCommand: GameCommand,
                              deck: Deck)

  case class StandResult(gameCommand: GameCommand,
                         deck: Deck)
  case class HitResult(optHands: Option[Hands],
                       deck: Deck,
                       action: Action)
  case class DoubleDownResult(gameCommand: GameCommand,
                              deck: Deck)

  //ゲーム開始のアナウンス
  private def startBlackJackOperatorSystem(): Unit ={
    println("Welcome to BLACK JACK Operator System\n")
  }

  //山札の取得
  private def getNewDeck: Deck ={
    val deckNum = readDeckNum()
    initDeck(deckNum,TRAMPS)
  }

  //手札の取得し山札から削除
  private def getHand(player: Player,deck: Deck): (Int,Deck) ={
    val hand = readHand(player)
    val deletedDeck = deleteHandsAtDeck(deck,hand)
    (hand,deletedDeck)
  }

  //コマンドの取得
  private def getGameCommand: GameCommand ={
    readGameCommand()
  }

  //ゲーム開始時のプレイヤーの手札を取得
  private def getPlayerHands(deck: Deck): (Hands,Deck) ={
    val result1 = getHand(User,deck)
    val playerHand1 = result1._1
    val deck1 = result1._2
    val result2 = getHand(User,deck1)
    val playerHand2 = result2._1
    val deck2 = result2._2
    val playerHands = Seq(playerHand1,playerHand2)
    (playerHands,deck2)
  }

  private def createHandString(hands: Hands, count: Int): String ={
    if (count == 0) "[" ++ createHandString(hands,count +1)
    else if (count == hands.length) {
      hands(count -1).toString ++ "]"
    }
    else hands(count -1).toString ++ "," ++ createHandString(hands,count +1)
  }

  //計算に使用するデータを標準出力
  private def announceDataToCalculate(playerHands: Hands,dealerHand: Int,deck: Deck): Unit ={

    val playerHandsString = createHandString(playerHands,0)
    println("\n入力情報を出力します。")
    println(s"Player's hand : $playerHandsString")
    println(s"Dealer's hand : [$dealerHand]")
    println(s"Number of decks : ${deck.length}")
  }

  //計算
  private def calculateAction(playerHands: Hands,dealerHand: Int,deck: Deck): Action ={
    //計算に使用するデータを標準出力
    announceDataToCalculate(playerHands,dealerHand,deck)
    //初期設定
    val originalData = OriginalData(playerHands,dealerHand,deck)
    //計算開始
    println("\nStart Calculating The Best Action...")
    val actionWithType = calculateHandsToAction(originalData)
    val calculationType = actionWithType._2
    val action = actionWithType._1
    calculationType match {
      case Refined => println("\n詳細分析完了")
      case Simplified => println("\n簡略分析完了")
    }
    //プレイヤーが選択したアクションを返却
    println(s"分析の結果、最善手は[${action.toString}]です。")
    val inputAction = readAction()
    inputAction
  }

  //Standコマンドのアクション
  private def standAction(deck: Deck): StandResult ={

    @tailrec
    def deleteDealerHands(deck:Deck, dealerHands: Hands, counter: Int): Deck ={
      if(counter < dealerHands.length) {
        val deletedDeck = deleteHandsAtDeck(deck,dealerHands(counter))
        deleteDealerHands(deletedDeck,dealerHands,counter +1)
      }
      else deck
    }
    val dealerHands: Hands = readDealerHands(Nil: Seq[Int])
    val dealerHandsString = createHandString(dealerHands,0)
    println(s"ディーラがHitしたトランプ : $dealerHandsString")
    val newDeck = deleteDealerHands(deck,dealerHands,0)
    val command = getGameCommand
    StandResult(command,newDeck)
  }

  //Hitコマンドのアクション
  private def hitAction(playerHands: Hands,dealerHand: Int,deck: Deck): HitResult ={
    val handWithDeck: (Int,Deck) = getHand(UserHit,deck)
    val hitHand = handWithDeck._1
    val deletedDeck = handWithDeck._2
    if(21 < (playerHands :+ hitHand).sum) {
      HitResult(None,deletedDeck,Stand)
    }
    else{
      val action = calculateAction(playerHands :+ hitHand,dealerHand,handWithDeck._2)
      HitResult(Some(playerHands :+ hitHand),deletedDeck,action)
    }
  }

  //DoubleDownのアクション
  private def doubleDownAction(deck: Deck): DoubleDownResult ={
    val deckDeletedPlayerHand = getHand(User,deck)._2
    val standResult = standAction(deckDeletedPlayerHand)
    DoubleDownResult(standResult.gameCommand,standResult.deck)
  }

  def firstFlow(deck: Deck): FirstFlowResult ={
    val playerHandsWithDeck: (Hands,Deck) = getPlayerHands(deck)
    val dealerHandWithDeck: (Int,Deck) = getHand(Dealer,playerHandsWithDeck._2)

    val playerHands = playerHandsWithDeck._1
    val dealerHand = dealerHandWithDeck._1
    val deletedDeck = dealerHandWithDeck._2
    val action = calculateAction(
      playerHandsWithDeck._1,
      dealerHandWithDeck._1,
      dealerHandWithDeck._2)

    FirstFlowResult(playerHands,dealerHand,deletedDeck,action)
  }

  @tailrec
  def actionFlow(playerHands: Option[Hands],
                   dealerHand: Option[Int],
                   deck: Deck,
                   action: Action): ActionFlowResult = action match {
    case Stand =>
      val standResult: StandResult = standAction(deck)
      ActionFlowResult(standResult.gameCommand, standResult.deck)

    case Hit =>
      val hitResult: HitResult = hitAction(playerHands.get,dealerHand.get,deck)
      actionFlow(
        hitResult.optHands,
        dealerHand,
        hitResult.deck,
        hitResult.action)

    case DoubleDown =>
      val doubleDownResult = doubleDownAction(deck)
      ActionFlowResult(doubleDownResult.gameCommand,doubleDownResult.deck)
  }

  //firstFlow and ActionSwitch
  def mainFlow(deck: Deck): MainFlowResult ={
    val firstFlowResult = firstFlow(deck)
    val actionFlowResult = actionFlow(
      Some(firstFlowResult.playerHands),
      Some(firstFlowResult.dealerHand),
      firstFlowResult.deck,
      firstFlowResult.action)

    MainFlowResult(actionFlowResult.gameCommand, actionFlowResult.deck)
  }

  @tailrec
  def commandSwitch(command: GameCommand, optDeck: Option[Deck]): Unit = command match {
    case StartGame =>
      startBlackJackOperatorSystem()
      val deck = getNewDeck
      val mainResult = mainFlow(deck)
      commandSwitch(
        mainResult.gameCommand,
        Some(mainResult.deck))

    case Continue =>
      val mainResult = mainFlow(optDeck.get)
      commandSwitch(
        mainResult.gameCommand,
        Some(mainResult.deck))

    case InitDeck =>
      val deck = getNewDeck
      val mainResult = mainFlow(deck)
      commandSwitch(
        mainResult.gameCommand,
        Some(mainResult.deck))

    case Finish => println("Shut Down The System.")
  }
}
