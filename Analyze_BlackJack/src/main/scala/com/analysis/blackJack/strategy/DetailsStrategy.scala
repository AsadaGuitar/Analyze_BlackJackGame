package com.analysis.blackJack.strategy

import com.analysis.blackJack._
import com.analysis.blackJack.strategy.DetailsStrategy
import com.analysis.blackJack.util.DeckUtili._
import com.analysis.common.calculation.{Probs, Rational}
import com.analysis.blackJack.util.HandUtil._
import com.analysis.blackJack.util.ActionUtil._
import com.analysis.blackJack.util.SystemCommandUtil._

import scala.language.{implicitConversions, postfixOps}

/*
オブジェクト名     DetailsStrategy
機能             Probsクラスから必要な確率を計算
*/
object DetailsStrategy {

  /*
  メソッド名     accuracyRate
  機能          確率の正確性を計算
  引数          implicit probs: Probs[Int]    使用するProbsクラス
  戻値          Option[Rational]              正確性を値として保有するRationalクラス
  */
  def accuracyRate(implicit probs: Probs[Int]) = probs.get(_ => true)

  /*
  メソッド名     dealerBurstRate
  機能          ディーラがバーストする確率を計算
  引数          implicit probs: Probs[Int]    使用するProbsクラス
  戻値          Option[Rational]              ディーラのバースト確率を値として保有するRationalクラス
  */
  def dealerBurstRate(implicit probs: Probs[Int]) = probs.get(21 < _)

  /*
  メソッド名     dealerWinRate
  機能          ディーラが今のユーザのスコアに勝つ確率を計算
  引数          userScore     : Int           ユーザスコア
  　　　　　　　　implicit probs: Probs[Int]    使用するProbsクラス
  戻値          Option[Rational]              ディーラの勝率を値として保有するRationalクラス
  */
  def dealerWinRate(userScore: Int)(implicit probs: Probs[Int]) =
    probs.get(x => x <= 21 && userScore < x)

  /*
  メソッド名     dealerLoseRate
  機能          ディーラが今のユーザのスコアに負ける確率を計算
  引数          userScore: Int      　　　　　　ユーザスコア
  　　　　　　　　implicit probs: Probs[Int]    使用するProbsクラス
  戻値          Option[Rational]              ディーラの負率を値として保有するRationalクラス
  */
  def dealerLoseRate(userScore: Int)(implicit probs: Probs[Int]): Option[Rational] = {

    val burstRational = probs.get(21 < _)
    val underRational = probs.get(_ < userScore)

    if (burstRational.isEmpty) {
      if (underRational.isEmpty) None
      else underRational
    }
    else {
      if (underRational.isEmpty) burstRational
      else for {
        u <- underRational
        b <- burstRational
      } yield u + b
    }
  }

  /*
  メソッド名     userBurstRate
  機能          ユーザがバーストする確率を計算
  引数          userScore: Int      　　　　　　ユーザスコア
  　　　　　　　　implicit probs: Probs[Int]    使用するProbsクラス
  戻値          Option[Rational]              ユーザバースト率を値として保有するRationalクラス
  */
  def userBurstRate(userScore: Int, deck: Deck)(implicit probs: Probs[Int]) = {
    val count: Int = deck.count(x => 21 < userScore + x)
    Some(new Rational(count, deck.length))
  }

  /*
  メソッド名     userWinRate
  機能          ユーザが今の手札で勝利する確率を計算
  引数　　　　　　userScore: Int      　　　　　　ユーザスコア
  　　　　　　　　implicit probs: Probs[Int]    使用するProbsクラス
  戻値          Option[Rational]              ユーザ勝率を値として保有するRationalクラス
  */
  def userWinRate(userScore: Int)(implicit probs: Probs[Int]) =
    for {
      burst <- probs.get(21 < _)
      under <- probs.get(_ < userScore)
    } yield burst + under

  /*
  メソッド名     userLoseRate
  機能          ユーザの手札が負ける確率を計算
  引数          userScore: Int      　　　　　　ユーザスコア
  　　　　　　　　implicit probs: Probs[Int]    使用するProbsクラス
  戻値          Option[Rational]              ユーザ負率を値として保有するRationalクラス
  */
  def userLoseRate(userScore: Int)(implicit probs: Probs[Int]) = probs.get(x => x <= 21 && userScore < x)

  /*
  メソッド名     drawRate
  機能          引き分けになる確率を計算
  引数          userScore: Int      　　　　　　ユーザスコア
  　　　　　　　　implicit probs: Probs[Int]    使用するProbsクラス
  戻値          Option[Rational]              引き分けになる確率を値として保有するRationalクラス
  */
  def drawRate(userScore: Int)(implicit probs: Probs[Int]) = probs.get(_ == userScore)

  /*
  メソッド名     getRate
  機能          OptionでラッピングされたRationalを受取り、
  　　　　　　　　値が存在する場合はRationalの浮動小数値を返却し、
  　　　　　　　　値が存在しない場合は0を返却
  引数          rational: Option[Rational]    値を取得するOptionでラッピングされたRational
  戻値          BigDecimal                    値を取り出した結果
  */
  def getRate(rational: Option[Rational]): BigDecimal = rational match {
    case Some(x) => x.get()
    case _ => 0
  }

  /*
  メソッド名     bestAction
  機能          確率に基づき最善手を返却
  引数          user: Hand                    使用中のユーザ手札
  　　　　　　　　deck: Deck                    使用中のディーラ手札
  　　　　　　　　implicit probs: Probs[Int]    使用するProbsクラス
  戻値          Action                        確率に基づき計算されたアクション
  */
  def bestAction(user: Hand, deck: Deck)(implicit probs: Probs[Int]): Action = {
    if      (getRate(dealerBurstRate) < 0.1)          Hit
    else if (getRate(dealerLoseRate(user.sum)) > 0.7) Stand
    else if (getRate(dealerWinRate(user.sum)) > 0.8)  Hit
    else                                              Stand
  }
}
