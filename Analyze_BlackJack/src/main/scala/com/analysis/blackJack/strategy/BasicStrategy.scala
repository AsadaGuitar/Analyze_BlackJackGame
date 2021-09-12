package com.analysis.blackJack.strategy

import com.analysis.blackJack._
import com.analysis.blackJack.util.HandUtil._
import com.analysis.blackJack.util.ActionUtil._
import com.analysis.blackJack.util.SystemCommandUtil._


/*
オブジェクト      BasicStrategy
機能            ベーシックストラテジーに基づき最善手を計算
*/
object BasicStrategy { 

  /*
  メソッド名       hardHand
  機能            ユーザスコア、ディーラスコアを受取り、最善手を計算
  引数            playerScore:  Int       ユーザスコア
  　　            dealerScore:  Int       ディーラスコア
  戻値            Action                  最善手として計算されたアクション
  */
  private def hardHand(playerScore: Int, dealerScore: Int): Action = playerScore match {
    case score if score <= 8 => Hit
    case score if 17 <= score => Stand
    case score if (13 to 16).contains(score) =>
      dealerScore match {
        case score if (2 to 6).contains(score) => Stand
        case _ => Hit
      }
    case 9 =>
      dealerScore match {
        case score if (3 to 6).contains(score) => DoubleDown
        case _ => Hit
      }
    case 10 =>
      dealerScore match {
        case score if score == 10 || score == 1 => Hit
        case _ => DoubleDown
      }
    case 11 =>
      dealerScore match {
        case 1 => Hit
        case _ => DoubleDown
      }
    case 12 =>
      dealerScore match {
        case score if (4 to 6).contains(score) => Stand
        case _ => Hit
      }
  }

  /*
  メソッド名     softHand
  機能          ユーザスコアとディーラスコアから最善手を計算
  引数          scoreNotAce:  Int         ユーザスコア
  　　          dealerScore:  Int         ディーラスコア
  戻値          Action                    最善手として計算されたアクション
  */
  private def softHand(scoreNotAce: Int, dealerScore: Int): Action = scoreNotAce match {
    case score if score == 4 || score == 5 =>
      dealerScore match {
        case score if (4 to 6).contains(score) => Stand
        case _ => Hit
      }
    case score if score == 8 || score == 9 => Stand
    case 2 =>
      dealerScore match {
        case score if score == 5 || score == 6 => DoubleDown
        case _ => Hit
      }
    case 3 =>
      dealerScore match {
        case 4 => Stand
        case score if score == 5 || score == 6 => DoubleDown
        case _ => Hit
      }
    case 6 =>
      dealerScore match {
        case score if (3 to 6).contains(score) => Stand
        case _ => Hit
      }
    case 7 =>
      dealerScore match {
        case score if (3 to 6).contains(score) => DoubleDown
        case score if Seq(2,7,8).contains(score) => Stand
        case _ => Hit
      }
  }

  /*
  メソッド名     bestAction
  機能          ユーザ手札とディーラ手札を受取り、最善手を返却
  引数          user  : Hand      ユーザ手札
  　　          dealer: Hand      ディーラ手札
  戻値          Action            最善手として計算されたアクション
  */
  def bestAction(user: Hand, dealer: Hand) = user match {
    //ユーザ手札に1が含まれていた場合softHandメソッドを実行
    case hand if hand.contains(1) =>
      val userScore = (user diff Seq(1)).sum
      val dealerScore = dealer.sum
      softHand(userScore, dealerScore)
    //ユーザ手札に1が含まれていない場合hardHandメソッドを実行
    case _ =>
      val userScore = user.sum
      val dealerScore = dealer.sum
      hardHand(userScore, dealerScore)
  }
}
