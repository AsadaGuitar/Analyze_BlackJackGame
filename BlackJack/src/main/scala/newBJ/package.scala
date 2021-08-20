package object newBJ {

  type Tramp = Int
  type Tramps = Seq[Tramp]
  type Deck = Seq[Tramp]

  abstract class SystemCommand
  case object Start extends SystemCommand
  case object End extends SystemCommand

  def initDeck(num: Int, tramps: Tramps): Deck = {
    def addTramps(num: Int, ts: Tramps): Deck = {
      if (num <= 1) ts
      else ts ++ addTramps(num - 1, ts)
    }
    if (num < 1) Nil
    else addTramps(num * 4, tramps)
  }
}
