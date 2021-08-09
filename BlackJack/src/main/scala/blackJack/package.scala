package object blackJack {

  type Deck = Seq[Int]
  type Hands = Seq[Int]

  val TRAMPS = Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10)

  abstract class Action
  case object Hit extends Action
  case object Stand extends Action
  case object DoubleDown extends Action

  abstract class GameCommand
  case object StartGame extends GameCommand
  case object Continue extends GameCommand
  case object InitDeck extends GameCommand
  case object Finish extends GameCommand
}
