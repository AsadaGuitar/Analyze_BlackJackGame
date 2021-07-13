object Tramp {

  abstract class Tramp(val name: String, val num: Int)
  case object Ace extends Tramp("ace", 1)
  case object Two extends Tramp("two", 2)
  case object Three extends Tramp("three", 3)
  case object Four extends Tramp("four", 4)
  case object Five extends Tramp("five", 5)
  case object Six extends Tramp("six", 6)
  case object Seven extends Tramp("seven", 7)
  case object Eight extends Tramp("eight", 8)
  case object Nine extends Tramp("nine", 9)
  case object Ten extends Tramp("ten", 10)
  case object Eleven extends Tramp("jack", 10)
  case object Twelve extends Tramp("queen", 10)
  case object Thirteen extends Tramp("king", 10)

}
