package blackjack.system.parts

import blackjack.calculation.statistical.probability.{ProbabilityOfBlackJack, Rational}

trait ProbsPrintable {

  private def printResultRational(rational: Option[Rational], text: String): Unit =
    rational match {
      case Some(x) => println(s"$text : ${x.get().toFloat * 100} %")
      case None => println(s"$text : 0 %")
    }

  def printProbs(probability: ProbabilityOfBlackJack): Unit ={

    printResultRational(probability.accuracyProb,"正確性")
    printResultRational(probability.dealerBurstProb,"ディーラバースト率")
    printResultRational(probability.dealerWinProb,"ディーラ勝率")
    printResultRational(probability.dealerLoseProb, "ディーラ負率")
    printResultRational(probability.userBurstProb,"ユーザバースト率")
    printResultRational(probability.userWinProb,"ユーザ勝率")

  }
}
