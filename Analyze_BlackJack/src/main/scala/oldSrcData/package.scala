import scala.annotation.tailrec

package object blackJack {

  type Tramp = Int
  type Deck = Vector[Tramp]
  type Hand = Seq[Tramp]

  abstract class Action

  case object Hit extends Action

  case object Stand extends Action

  case object DoubleDown extends Action

  abstract class SystemCommand

  case object Continue extends SystemCommand

  case object Init extends SystemCommand

  case object Finish extends SystemCommand
  
}
