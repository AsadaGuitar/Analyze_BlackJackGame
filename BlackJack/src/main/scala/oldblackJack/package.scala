import scala.annotation.tailrec

/*
パッケージオブジェクト
機能    パッケージ内で使う情報を格納
 */
package object oldblackJack {

  //エイリアスを定義
  type Deck = Seq[Int]
  type Hands = Seq[Int]
  type Tramp = Int
  type Tramps = Seq[Tramp]

  //アクションを定義
  abstract class Action
  case object Hit extends Action
  case object Stand extends Action
  case object DoubleDown extends Action

  //コマンドを定義
  abstract class GameCommand
  case object StartGame extends GameCommand
  case object Continue extends GameCommand
  case object InitDeck extends GameCommand
  case object Finish extends GameCommand

  //計算の種類を定義
  abstract class CalculationType
  case object Simplified extends CalculationType
  case object Refined extends CalculationType

  //使用するトランプを定義
  val TRAMPS = Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10)

  /*
  メソッド名   initDeck
  機能        山札の初期化
  引数        deckNum: Int        使用する山札の個数
             tramps: Seq[Int]    使用するトランプの種類
  返値        Deck                初期化した山札
   */
  def initDeck(deckNum: Int, tramps: Seq[Int]): Deck = {
    //個数分を追加
    def addTramps(num: Int, ts: Seq[Int]): Seq[Int] = {
      if (num <= 1) ts
      else ts ++ addTramps(num - 1, ts)
    }
    //個数が0の場合
    if (deckNum < 1) Nil
    //山札の作成
    else addTramps(deckNum * 4, tramps)
  }

  /*
  メソッド名   deleteHandsAtDeck
  機能        山札からトランプを削除
  引数        deck: Deck      使用している山札
       　　　　hand: Int       削除するトランプ
  返値        Deck            トランプを削除した山札
   */
  def deleteHandsAtDeck(deck: Deck,hand: Int): Deck ={
    deck.diff(Seq(hand))
  }

  /*
  メソッド名   changeAceToEleven
  機能        Aceを11に変換し、範囲内に含まれる場合返却
  引数        aceCount: Int                 Aceの枚数
             hands: Hands                  手札
             isInRange: Int => Boolean     範囲
   */
  @tailrec
  def changeAceToEleven(aceCount: Int, hands: Hands, isInRange: Int => Boolean): Option[Hands] = {
    //AceCountが0になった場合
    if (aceCount < 1) None
    //含まれている場合
    else if ((17 to 21).contains(hands.sum + 10 * aceCount)) Some(hands :+ 10 * aceCount)
    //含まれていない場合
    else changeAceToEleven(aceCount - 1, hands, isInRange)
  }
}
