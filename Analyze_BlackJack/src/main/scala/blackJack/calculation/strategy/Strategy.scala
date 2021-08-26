package blackJack.calculation.strategy

import blackJack._

abstract class Strategy {
  def nextAction(): Action
}
