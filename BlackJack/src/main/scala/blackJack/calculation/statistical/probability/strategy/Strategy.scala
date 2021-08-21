package blackjack.calculation.statistical.probability.strategy

import blackjack._

abstract class Strategy {

  def nextAction(): Action
}
