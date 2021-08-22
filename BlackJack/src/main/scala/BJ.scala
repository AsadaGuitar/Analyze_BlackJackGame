import oldblackJack.BlackJackIO.{Dealer, Player, User, UserHit, readAction, readDealerHands, readDeckNum, readGameCommand, readHand}
import oldblackJack.BlackJackUtility.calculateHandsToAction
import oldblackJack.DetailedStrategy.OriginalData
import oldblackJack.{Action, Continue, Deck, DoubleDown, Finish, GameCommand, Hands, Hit, InitDeck, Refined, Simplified, Stand, StartGame, TRAMPS, deleteHandsAtDeck, initDeck}

import scala.annotation.tailrec

/*
オブジェクト名   Main
機能           ブラックジャックパッケージを使用したメインオブジェクト
 */
object BJ extends App {

  /*
  クラス名    StandResult
  機能       スタンドアクションを提供する関数の返値
  メンバ     gameCommand: GameCommand    分析終了後に入力されたコマンド
            deck: Deck                  今回のゲームで使用した山札
   */
  case class StandResult(gameCommand: GameCommand,
                         deck: Deck)
  /*
  クラス名    HitResult
  機能       ヒットアクションを提供する関数の返値
  メンバ     optHands: Option[Hands]　　アクション後の手札、バーストした場合はNone
            deck: Deck                アクション後の山札
            action: Action　　　　　　　 計算後に入力されたアクション、バーストした場合Stand
   */
  case class HitResult(optHands: Option[Hands],
                       deck: Deck,
                       action: Action)
  /*
  クラス名    DoubleDownResult
  機能       ダブルダウンアクションを提供する関数の返値
  メンバ      gameCommand: GameCommand   分析終了後に入力されたコマンド
             deck: Deck                 今回のゲームで使用した山札
   */
  case class DoubleDownResult(gameCommand: GameCommand,
                              deck: Deck)

  /*
  クラス名    FirstFlowResult
  機能       入力された手札を使用し最善のアクションを計算
  メンバ     playerHands: Hands     入力されたプレイヤーの手札
            dealerHand: Int        入力されたディーラの手札
            deck: Deck             入力された個数分の山札
            action: Action         計算結果のアクション
   */
  case class FirstFlowResult(playerHands: Hands,
                             dealerHand: Int,
                             deck: Deck,
                             action: Action)
  /*
  クラス名    ActionFlowResult
  機能       アクション[Stand,Hit,DoubleDown]を受取り、各々に対応した制御を提供する関数の返値
  メンバ     gameCommand: GameCommand    分析終了後に入力されたコマンド
            deck: Deck                  今回のゲームで使用した山札
   */
  case class ActionFlowResult(gameCommand: GameCommand,
                              deck: Deck)
  /*
  クラス名    MainFlowResult
  機能       ゲームの分析を提供するメイン関数の返値
  メンバ     gameCommand: GameCommand    分析終了後に入力されたコマンド
            deck: Deck                  今回のゲームで使用した山札
   */
  case class MainFlowResult(gameCommand: GameCommand,
                            deck: Deck)

  /*
  メソッド名   startBlackJackOperatorSystem
  機能        ゲーム開始のアナウンス
   */
  private def startBlackJackOperatorSystem(): Unit ={
    println("Welcome to BLACK JACK Operator System\n")
  }

  /*
  メソッド名   getNewDeck
  機能        山札を初期化し返却
  返値        Deck      初期化した山札
   */
  private def getNewDeck: Deck ={
    //デッキ個数の取得
    val deckNum = readDeckNum()
    //デッキの取得
    initDeck(deckNum,TRAMPS)
  }

  /*
  メソッド名   手札の取得し山札から削除
  機能        手札を入力させ、その手札を山札から抜き、手札と山札を返却
  引数        player: Player    手札の持ち主
             deck: Deck        使用している山札
  返値        (Int,Deck)
             Int               入力された手札
             Deck              入力された手札を削除した山札
   */
  private def getHand(player: Player,deck: Deck): (Int,Deck) ={
    //手札の取得
    val hand = readHand(player)
    //山札から手札を削除
    val deletedDeck = deleteHandsAtDeck(deck,hand)
    (hand,deletedDeck)
  }

  /*
  メソッド名   getGameCommand
  機能        コマンドの入力
  返値        GameCommand   分析終了後のコマンド
   */
  private def getGameCommand: GameCommand ={
    //コマンドの取得
    readGameCommand()
  }

  /*
  メソッド名   getPlayerHands
  機能        ゲーム開始時のプレイヤーの手札とその分を抜いた山札を返却
  引数        deck: Deck      使用している山札
  返値        (Hands,Deck)
             Hands           プレイヤーの手札
             Deck            プレイヤーの手札を抜いた山札
   */
  private def getPlayerHands(deck: Deck): (Hands,Deck) ={
    //手札を取得し山札から削除
    val result1 = getHand(User,deck)
    val playerHand1 = result1._1
    val deck1 = result1._2
    val result2 = getHand(User,deck1)
    val playerHand2 = result2._1
    val deck2 = result2._2
    val playerHands = Seq(playerHand1,playerHand2)
    (playerHands,deck2)
  }

  /*
  メソッド名   createHandString
  機能        手札を標準出力する際のフォーマット
  引数        hands: Hands    出力する手札のリスト
             count: Int     　手札のリストの開始位置（基本は0)
  返値        String          フォーマットに基づいて作成した文字列
   */
  private def createHandString(hands: Hands, count: Int): String ={
    //一つ目の要素の時に"["を追加
    if (count == 0) "[" ++ createHandString(hands,count +1)
    //最後尾の要素に"]"を追加
    else if (count == hands.length) hands(count -1).toString ++ "]"
    //最初と最後以外の要素の末尾に","を追加
    else hands(count -1).toString ++ "," ++ createHandString(hands,count +1)
  }

  /*
  メソッド名   announceDataToCalculate
  機能        入力されたプレイヤーの手札、ディーラの手札、山札を標準出力
  引数        playerHands: Hands    入力されたプレイヤーの手札
             dealerHand: Int       入力されたディーラの手札
             deck: Deck            入力された山札
   */
  private def announceDataToCalculate(playerHands: Hands,dealerHand: Int,deck: Deck): Unit ={
    //手札を表示する文字列の取得
    val playerHandsString = createHandString(playerHands,0)
    //標準出力
    println("\n入力情報を出力します。")
    println(s"Player's hand : $playerHandsString")
    println(s"Dealer's hand : [$dealerHand]")
    println(s"Number of decks : ${deck.length}")
  }

  /*
  メソッド名   calculateAction
  機能        入力情報を元に最善手を計算
  引数        playerHands: Hands    入力されたプレイヤーの手札
             dealerHand: Int       入力されたディーラの手札
             deck: Deck            入力された山札
  返値        Action                計算結果のアクション
   */
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

  /*
  メソッド名   standAction
  機能        スタンドアクション選択時の機能
  引数        deck: Deck    使用している山札
  返値        StandResult   選択されたコマンドと山札を返却
   */
  private def standAction(deck: Deck): StandResult ={

    //ディーラの手札を山札から削除
    @tailrec
    def deleteDealerHands(deck:Deck, dealerHands: Hands, counter: Int): Deck ={
      if(counter < dealerHands.length) {
        val deletedDeck = deleteHandsAtDeck(deck,dealerHands(counter))
        deleteDealerHands(deletedDeck,dealerHands,counter +1)
      }
      else deck
    }
    //ディーラの手札を取得
    val dealerHands: Hands = readDealerHands(Nil: Seq[Int])
    //ディーラの手札を表示する際に使用する文字列を取得
    val dealerHandsString = createHandString(dealerHands,0)
    //標準出力
    println(s"ディーラがHitしたトランプ : $dealerHandsString")
    //山札からディーラの手札を削除
    val newDeck = deleteDealerHands(deck,dealerHands,0)
    //コマンドの取得
    val command = getGameCommand
    StandResult(command,newDeck)
  }

  /*
  メソッド名   hitAction
  機能        ヒットアクション選択時の機能
  引数        playerHands: Hands    プレイヤーの手札
             dealerHand: Int       ディーラの手札
             deck: Deck            使用している山札
  返値        HitResult             ヒット後の手札、削除後の山札、入力されたアクションを返却
   */
  private def hitAction(playerHands: Hands,dealerHand: Int,deck: Deck): HitResult ={
    //山札からトランプをHit
    val handWithDeck: (Int,Deck) = getHand(UserHit,deck)
    val hitHand = handWithDeck._1
    val deletedDeck = handWithDeck._2
    //バーストしているか
    if(21 < (playerHands :+ hitHand).sum) {
      //バーストしている場合、山札とStandコマンドを返却
      HitResult(None,deletedDeck,Stand)
    }
    else{
      //バーストしていない場合、アクション入力にてアクションを取得し、手札、山札、アクションを返却
      val action = calculateAction(playerHands :+ hitHand,dealerHand,handWithDeck._2)
      HitResult(Some(playerHands :+ hitHand),deletedDeck,action)
    }
  }

  /*
  メソッド名   doubleDownAction
  機能        ダブルダウンアクション選択時の機能
  引数        deck: Deck            使用している山札
  返値        DoubleDownResult      選択されたコマンドと、山札を返却
   */
  private def doubleDownAction(deck: Deck): DoubleDownResult ={
    //プレイヤーがHitした後の山札を取得
    val deckDeletedPlayerHand = getHand(User,deck)._2
    //standActionを実行
    val standResult = standAction(deckDeletedPlayerHand)
    DoubleDownResult(standResult.gameCommand,standResult.deck)
  }

  /*
  メソッド名   firstFlow
  機能        手札を取得し計算結果を表示、その後アクションを取得
  引数        deck: Deck            使用している山札
  返値        FirstFlowResult       プレイヤーの手札、ディーラの手札、山札、入力されたアクションを返却
   */
  def firstFlow(deck: Deck): FirstFlowResult ={
    //プレイヤーの手札と山札を取得
    val playerHandsWithDeck: (Hands,Deck) = getPlayerHands(deck)
    //ディーラの手札と山札を取得
    val dealerHandWithDeck: (Int,Deck) = getHand(Dealer,playerHandsWithDeck._2)

    val playerHands = playerHandsWithDeck._1
    val dealerHand = dealerHandWithDeck._1
    val deletedDeck = dealerHandWithDeck._2
    //計算を実行し、結果を格納
    val action = calculateAction(
      playerHands,
      dealerHand,
      deletedDeck)

    FirstFlowResult(playerHands,dealerHand,deletedDeck,action)
  }

  /*
  メソッド名   actionFlow
  機能        受取ったアクションに対応した制御を提供
  引数        playerHands: Option[Hands]    プレイヤーの手札
             dealerHand: Option[Int]       ディーラの手札
             deck: Deck                    使用している山札
             action: Action                アクション
  返値        ActionFlowResult              入力されたコマンドと山札を返却
   */
  @tailrec
  def actionFlow(playerHands: Option[Hands],
                 dealerHand: Option[Int],
                 deck: Deck,
                 action: Action): ActionFlowResult = action match {
    //Standアクションの実行
    case Stand =>
      val standResult: StandResult = standAction(deck)
      ActionFlowResult(standResult.gameCommand, standResult.deck)

    //Hitアクションの実行
    case Hit =>
      val hitResult: HitResult = hitAction(playerHands.get,dealerHand.get,deck)
      actionFlow(
        hitResult.optHands,
        dealerHand,
        hitResult.deck,
        hitResult.action)

    //DoubleDownアクションの実行
    case DoubleDown =>
      val doubleDownResult = doubleDownAction(deck)
      ActionFlowResult(doubleDownResult.gameCommand,doubleDownResult.deck)
  }

  /*
  メソッド名   mainFlow
  機能        firstFlowとactionFlowを合併させ一回分のゲーム分析を提供
  引数        deck: Deck            使用する山札
  返値        MainFlowResult        入力されたコマンドと山札を返却
   */
  def mainFlow(deck: Deck): MainFlowResult ={
    //firstFlowを実行し、結果を格納
    val firstFlowResult = firstFlow(deck)
    //actionFlowを実行し、結果を格納
    val actionFlowResult = actionFlow(
      Some(firstFlowResult.playerHands),
      Some(firstFlowResult.dealerHand),
      firstFlowResult.deck,
      firstFlowResult.action)

    MainFlowResult(actionFlowResult.gameCommand, actionFlowResult.deck)
  }

  /*
  メソッド名   commandSwitch
  機能        ゲームコマンドを受取り、対応した制御を提供
  引数        command: GameCommand    使用するゲームコマンド
             optDeck: Option[Deck]　　使用する山札、起動時はNone
   */
  @tailrec
  def commandSwitch(command: GameCommand, optDeck: Option[Deck]): Unit = command match {
    //StartGameコマンドの実行
    case StartGame =>
      //処理開始のアナウンス
      startBlackJackOperatorSystem()
      //山札の取得
      val deck = getNewDeck
      //ゲーム分析開始
      val mainResult = mainFlow(deck)
      //分析結果後のコマンドを実行
      commandSwitch(
        mainResult.gameCommand,
        Some(mainResult.deck))

    //Continueコマンドの実行
    case Continue =>
      //ゲーム分析開始
      val mainResult = mainFlow(optDeck.get)
      //分析結果後のコマンドを実行
      commandSwitch(
        mainResult.gameCommand,
        Some(mainResult.deck))

    //InitDeckコマンドの実行
    case InitDeck =>
      //山札の取得
      val deck = getNewDeck
      //ゲーム分析開始
      val mainResult = mainFlow(deck)
      //分析結果後のコマンドを実行
      commandSwitch(
        mainResult.gameCommand,
        Some(mainResult.deck))

    //Finishコマンドの実行
    case Finish =>
      //処理を終了
      println("Shut Down The System.")
  }

  //処理を開始
  commandSwitch(StartGame,None)
}
