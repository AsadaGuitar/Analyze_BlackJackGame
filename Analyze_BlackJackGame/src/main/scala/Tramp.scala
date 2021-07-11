object Tramp {

  abstract class AbsTramp(val name: String, val num: Int)
  case object Ace extends AbsTramp("ace", 1)
  case object Two extends AbsTramp("two", 2)
  case object Three extends AbsTramp("three", 3)
  case object Four extends AbsTramp("four", 4)
  case object Five extends AbsTramp("five", 5)
  case object Six extends AbsTramp("six", 6)
  case object Seven extends AbsTramp("seven", 7)
  case object Eight extends AbsTramp("eight", 8)
  case object Nine extends AbsTramp("nine", 9)
  case object Ten extends AbsTramp("ten", 10)
  case object Eleven extends AbsTramp("jack", 10)
  case object Twelve extends AbsTramp("queen", 10)
  case object Thirteen extends AbsTramp("king", 10)

}
