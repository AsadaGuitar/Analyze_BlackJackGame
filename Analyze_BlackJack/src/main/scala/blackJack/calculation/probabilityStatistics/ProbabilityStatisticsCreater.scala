package blackJack.calculation.probabilityStatistics

import blackJack.{Deck, Hand}
import blackJack.calculation.Rational

import collection.parallel.CollectionConverters.VectorIsParallelizable
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global


/*
トレイト名     ProbabilityStatisticsCreater
機能          確率統計を生成
*/
trait ProbabilityStatisticsCreater[T] {

  /*
  メソッド名   create
  機能        確率統計の生成
  引数        implicit timeout: Duration                                       タイムアウトの設定
  戻値        Either[concurrent.TimeoutException,ProbabilityStatistics[T]]      生成した確率統計
  */
  def create(implicit timeout: Duration): Either[concurrent.TimeoutException,ProbabilityStatistics[T]]
}

/*
クラス名      HandProbabilityStatisticsCreater
機能         手札の確率統計を生成
引数         dealerHand: Hand       ディーラの手札
            usingDeck: Deck         使用中の山札
*/
class HandProbabilityStatisticsCreater(dealerHand: Hand, usingDeck: Deck) 
  extends ProbabilityStatisticsCreater[Int] 
    with HandProbabilityCalculator 
    with HandStatisticsCreater {
  
  /*
  メソッド名   create
  機能        手札の確率統計を生成
  引数        implicit timeout: Duration                                          タイムアウトの設定
  戻値        Either[concurrent.TimeoutException,ProbabilityStatistics[Int]]      生成した確率統計
  */
  override def create(implicit timeout: Duration): Either[concurrent.TimeoutException,ProbabilityStatistics[Int]]={

    //統計を取得し、各情報を集計
    @throws[concurrent.TimeoutException]
    def probsList(): Seq[(Int,Rational)] = {
      //統計を並列、非同期処理で取得
      val asynchronousResult: Future[Seq[Seq[Hand]]] = paralellLoopHit(dealerHand,usingDeck)
      //非同期の待機時間が設定されたタイムアウトの時間を超過した場合、例外が発生
      val statistics = Await.result(asynchronousResult, timeout)
      //統計の確率を集計
      for {
        hand <- statistics.flatten
      } yield (hand.sum, calculateProb(hand,usingDeck))
    }
    
    try {
      //確率統計を生成
      val probs = probsList()
      Right(ProbabilityStatistics(probs))
    }
    catch {
      //確率統計生成時、例外が発生した場合はEither.Leftを返却
      case e : concurrent.TimeoutException => Left(e)
    }
  }
}