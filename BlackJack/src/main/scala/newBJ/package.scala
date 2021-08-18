package object newBJ {

  type Tramp = Int
  type Hand = Seq[Tramp]
  type Deck = Seq[Tramp]

  def productOfList(ints: List[Int]): BigInt = {
    def loop(ints: List[Int]): BigInt = ints match {
      case Nil => 1
      case head :: tail => head * loop(tail)
    }
    if (ints.isEmpty) 0
    else loop(ints)
  }

  def initDeck(num: Int, tramps: Seq[Int]): Deck = {
    def addTramps(num: Int, ts: Seq[Int]): Seq[Int] = {
      if (num <= 1) ts
      else ts ++ addTramps(num - 1, ts)
    }
    if (num < 1) Nil
    else addTramps(num * 4, tramps)
  }
}
