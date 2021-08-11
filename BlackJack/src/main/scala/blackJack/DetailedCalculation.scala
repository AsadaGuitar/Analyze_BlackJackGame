package blackJack

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/*
オブジェクト名   DetailedCalculation
機能           ディーラが引く可能性のある組み合わせを全件取得
 */
object DetailedCalculation {

  /*
  クラス名    Rations
  機能       計算結果を保持
  メンバ      score: Int                   スコア
             probability: BigDecimal      確率
             calculatedCount: Long        ループした回数
   */
  case class Rations(score: Int, probability: BigDecimal, calculatedCount: Long)

  /*
  メソッド名   productOfList
  機能        リストの要素を掛け算する
  引数        ints: List[Int]   計算するリスト
  戻値        BigInt            計算結果
   */
  private def productOfList(ints: List[Int]): BigInt = {
    def loop(ints: List[Int]): BigInt = ints match {
      case Nil => 1
      case head :: tail => head * loop(tail)
    }
    if (ints.isEmpty) 0
    else loop(ints)
  }

  /*
  関数名   hitRatioByLengthCalculation
  機能     手札と山札の長さを受取って確率を計算
  引数     len: Int         手札の長さ
          deckLen: Int     山札の長さ
  戻値     BigDecimal       計算結果の浮動小数
   */
  val hitRatioByLengthCalculation: (Int, Int) => BigDecimal = {
    (len: Int, deckLen: Int) =>
      val r: List[Int] = (((deckLen - len) + 1) to deckLen).toList
      1 / productOfList(r).toDouble
  }

  /*
  関数名   denominatorCalculation
  機能     手札と山札の長さを受取って確率の分母を計算
  引数     len: Int         手札の長さ
          deckLen: Int     山札の長さ
  戻値     BigInt       計算結果
   */
  val denominatorCalculation: (Int, Int) => BigInt = {
    (len: Int, deckLen: Int) =>
      val r: List[Int] = (((deckLen - len) + 1) to deckLen).toList
      val denominator = productOfList(r)
      denominator
  }

  /*
  関数名   loopHit
  機能     将来的に引くスコアとその確率を全て計算しリストにして返却
  引数     hands: Hands               手札
          deck: Deck                  山札
          parentDeck: Deck            元の山札
          hitRange: Int => Boolean    取得するスコアの範囲
          calculatedCount: Long       計算回数
   */
  val loopHit: (Hands,Deck,Deck,Int => Boolean,Long) => Seq[Rations] =
    (hands: Hands,
     deck: Deck,
     parentDeck: Deck,
     hitRange: Int => Boolean,
     calculatedCount: Long) => {

      val score: Int = hands.sum
      val count: Long = calculatedCount + 1

      //スコアが17以上の場合
      if (17 <= score) {
        //リストの長さから確率を取得
        val denominator = hitRatioByLengthCalculation(hands.length -1, parentDeck.length)
        Seq(Rations(score,denominator,count))
      }
      //スコアが17未満で手札に1が含まれている場合
      else if(hands.contains(1)) {
        //1を11に変換し取得するスコアの範囲に含まれている場合Some(x)、含まれていない場合None
        changeAceToEleven(hands.count(_ == 1), hands, hitRange) match {
          //取得するスコアの範囲に含まれている場合
          case Some(x) =>
            val score = x.sum
            //リストの長さから確率を取得
            val denominator = hitRatioByLengthCalculation(x.length - 2, parentDeck.length)
            Seq(Rations(score, denominator, count))
          //含まれていない場合
          case None =>
            //山札の枚数分再帰
            for {
              t <- deck
              l <- loopHit(hands :+ t, deck.diff(Seq(t)), parentDeck, hitRange, count)
            } yield l
        }
      }
      //スコアが17未満の場合
      else {
        //山札の枚数分再帰
        for{
          t <- deck
          l <- loopHit(hands :+ t,deck.diff(Seq(t)),parentDeck,hitRange,count)
        } yield l
      }
    }

  /*
  関数名   futureLoopHit
  機能     loopHitの非同期実行
  引数     hands: Hands                 計算する手札
          deck: Deck                   使用している山札
          parentDeck: Deck             使用している山札
          isInRange: Int => Boolean    計算するスコアの範囲
  戻値     Future[Seq[Rations]]         非同期実行の結果
   */
  val futureLoopHit: (Hands, Deck, Deck, Int => Boolean) => Future[Seq[Rations]] =
    (hands: Hands,
     deck: Deck,
     parentDeck: Deck,
     isInRange: Int => Boolean) =>
      Future(loopHit(hands,deck,parentDeck,isInRange,0))

  /*
  関数名   asynchronousCalculationSeparated
  機能     山札の枚数分非同期実行
  引数     hands: Hands                  計算する手札
          deck: Deck                    使用している山札
          parentDeck: Deck              使用している山札
          isInRange: Int => Boolean     計算するスコアの範囲
   */
  val asynchronousCalculationSeparated:
    (Hands, Deck, Deck, Int => Boolean)
      => Future[Seq[Seq[Rations]]] =
    (hands: Hands,
     deck: Deck,
     parentDeck: Deck,
     isInRange: Int => Boolean)  => {

      val futures: Seq[Future[Seq[Rations]]] =
        for{
          t <- deck
        } yield futureLoopHit(hands :+ t, deck.diff(Seq(t)),parentDeck,isInRange)
      //型の整合性を合わせる
      Future.sequence(futures)
    }
}
