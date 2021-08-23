package object blackjack {


  type Tramp = Int
  type Tramps = Seq[Tramp]
  type Deck = Seq[Int]

  abstract class Action

  case object Hit extends Action

  case object Stand extends Action

  case object DoubleDown extends Action

  abstract class SystemCommand

  case object Continue extends SystemCommand

  case object InitDeck extends SystemCommand

  case object Finish extends SystemCommand

}
