package blackJack

import Analyze_BlackJack_IO.*
import blackJack.calculation.Rational
import blackJack.calculation.probabilityStatistics.{HandProbabilityStatisticsCreater, ProbabilityStatistics}
import blackJack.calculation.strategy.{BasicStrategy, DetailsStrategy}

import scala.concurrent.duration.*
import scala.language.postfixOps

import scala.concurrent.ExecutionContext.Implicits.global


/*
オブジェクト名     Analyze_BlackJack
機能             プレイ中のブラックジャックにおいて最善手を計算
説明             blackJack¥package.scalaにてエイリアス、アクション、コマンドを定義
*/
object Analyze_BlackJack {

  /*
  メソッド名   createDeck
  機能        ゲームで使用する山札の個数を受取り、山札を生成
  引数        deckNum: Int       山札の個数
  戻値        Option[Deck]    使用する山札
  */
  private def createDeck(deckNum: Int): Option[Deck] = {
    //トランプの組合わせを定義
    val deck: Deck = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10)

    //個数分、トランプの組合わせを追加
    def addTramps(num: Int, deck: Deck): Deck = {
      if (num <= 1) deck
      else deck ++ addTramps(num - 1, deck)
    }

    //個数が0の場合空の山札を返却
    if (deckNum < 1) None
    else Some(addTramps(deckNum * 4, deck))
  }


  def main(args: Array[String]): Unit = {

    println("Welcome to Analyze_BlackJack")

    /*
    メソッド名   init
    機能        システム(山札)の初期化
                mainFlowの実行
    */
    def init(): Unit = {

      //山札を生成
      println("[山札初期化] : 山札の個数を入力してください。")
      val deckNum = readInt()
      val deck = createDeck(deckNum)

      deck match {
        //山札が存在する場合、mainFlowを実行
        case Some(x) => mainFlow(x)
        //山札が存在しない場合、再度実行
        case None =>
          println("山札が存在しません。")
          init()
      }
    }

    /*
    メソッド名     calculate
    機能          ディーラのスコアの確率統計を作成、分析し、最善手を返却
    引数          user: Hand        ユーザの手札
    　　          dealer: Hand      ディーラの手札
    　　          deck: Deck        使用中の山札
    戻値          Action            確率統計に基づいて算出された最善手
    */
    def calculate(user: Hand, dealer: Hand, deck: Deck): Action = {
      println("計算を開始します。")

      val probabilityStatisticsCreater = new HandProbabilityStatisticsCreater(dealer, deck)
      val start = System.currentTimeMillis()
      val probabilityStatistics = probabilityStatisticsCreater.create(timeout = 1 second)
      val end = System.currentTimeMillis()
      println(s"${(end - start) / 1000.toDouble}s")

      probabilityStatistics match {
        case Right(x) =>
          println(x)
          val detailsStrategy = new DetailsStrategy(x)
          detailsStrategy.bestAction(user, deck)
        case Left(e) =>
          println("タイムアウトしました")
          BasicStrategy.bestAction(user, dealer)
      }
    }

    /*
    メソッド名     actionFlow
    機能          入力されたアクションの処理を実行
    引数          user: Hand        ユーザの手札
    　　          dealer: Hand      ディーラの手札
    　　          deck: Deck        使用中の山札
    　　          action: Action    入力されたアクション
    戻値          Deck              アクション後の山札
    */
    def actionFlow(user: Hand, dealer: Hand, deck: Deck, action: Action): Deck = action match {
      case Hit =>
        //ユーザがヒットした手札を取得
        println("[手札入力] : ユーザの手札を入力してください。")
        val hitHand = readHand()
        //現在の手札を取得
        val afterHitHand = user ++ hitHand
        //ヒット分を削除した山札を取得
        val deckDeleted = deck diff hitHand

        //ディーラのスコアの確率統計を作成、分析し、最善手を取得
        val nextAction = calculate(afterHitHand, dealer, deckDeleted)
        println(s"最善手は${nextAction}です。")

        //ユーザが選択したアクションを取得
        println("[アクション入力] : \"Hit\",\"DoubleDown\",\"Stand\"のいずれかを入力してください。")
        val userAction = readAction()
        //アクションを実行
        actionFlow(afterHitHand, dealer, deckDeleted, userAction)

      //山札をそのまま返却
      case Stand => deck

      case DoubleDown =>
        //ユーザがヒットした手札を取得
        println("[手札入力] : ユーザの手札を入力してください。")
        val hitHand = readHand()
        //ヒット分を削除した山札を返却
        val deckDeleted = deck diff user
        deckDeleted
    }

    /*
    メソッド名     mainFlow
    機能          プレイ中のブラックジャックにおいて最善手を計算
    引数          deck: Deck    使用中の山札
    */
    def mainFlow(deck: Deck): Unit = {

      //ユーザの手札を取得、取得分の手札を山札から削除
      println("[手札入力] : ユーザの手札を一枚ずつ入力してください。")
      val userHandA = readHand()
      val deckA = deck diff userHandA
      println("[手札入力] : ユーザの手札を一枚ずつ入力してください。")
      val userHandB = readHand()
      val deckB = deckA diff userHandB

      //ディーラの手札を取得、取得分の手札を山札から削除
      println("[手札入力] : ディーラの手札を入力してください。")
      val dealerHand = readHand()
      val deckC = deckB diff dealerHand

      //ディーラのスコアの確率統計を作成、分析し、最善手を出力
      val action = calculate(userHandA ++ userHandB, dealerHand, deckC)
      println(s"最善手は${action}です。")

      //ユーザが選択したアクションを取得
      println("[アクション入力] : \"Hit\",\"DoubleDown\",\"Stand\"のいずれかを入力してください。")
      val userAction = readAction()

      //アクション後の山札を取得
      val deckAfterAction = actionFlow(userHandA ++ userHandB, dealerHand, deckC, userAction)

      //ディーラがHitしたトランプを取得
      println("ディーラがHitした手札を一枚ずつ入力してください。\n終了する場合は\"q\"を入力してください。")
      val dealerHit = readDealerHitTramps()
      //ディーラの最終的な手札を出力
      val finalDealerHand = dealerHand ++ dealerHit
      println(s"ディーラの手札は${finalDealerHand.mkString("[",",","]")}でした。")

      //ゲーム終了後の山札を取得
      val deckCompleted = deckAfterAction diff dealerHit

      //システムコマンドの取得、実行
      println("[コマンド入力] : \"Continue\",\"Init\",\"Finish\"のいずれかを入力してください。")
      readSystemCommand() match {
        //山札をそのままで再度実行
        case Continue => mainFlow(deckCompleted)
        //山札を初期化し再度実行
        case Init => init()
        //処理を終了
        case Finish => println("終了します。")
      }
    }
    
    //山札を初期化し、処理を開始
    init()
  }
}
