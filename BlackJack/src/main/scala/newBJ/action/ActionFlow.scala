package newBJ.action

object ActionFlow {

  sealed abstract class Action
  case object Hit extends Action
  case object DoubleDown extends Action
  case object Stand extends Action

  def create(action: Action): ActionFlow = action match {
    case Hit => new HitFlow()
    case DoubleDown => new DoubleDownFlow()
    case Stand => new StandFlow()
  }
}

abstract class ActionFlow