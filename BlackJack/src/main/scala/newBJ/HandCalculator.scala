package newBJ

object HandCalculator {

  def findFutureHand(player: Player, deck: Deck): Seq[Player] = {
    if(17 <= player.hand.sum) Seq(player)
    else {
      for{
        d <- deck
        newPlayer = new Player(player.hand :+ d)
        exPlayer = newPlayer.exchangeAce((17 to 21).contains)
        f <- findFutureHand(exPlayer, deck diff Seq(d))
      } yield f
    }
  }
}
