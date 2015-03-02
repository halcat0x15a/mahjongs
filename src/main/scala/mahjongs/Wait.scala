package mahjongs

sealed abstract class Wait(val fu: Int)

object Wait {
  def find(winning: Int, hand: Seq[Meld]): Option[Wait] =
    hand.flatMap(meld => meld.tiles.map(_ -> meld)).lift(winning).collect {
      case (_, Meld.Set(_, 2)) => Single
      case (_, Meld.Set(_, 3)) => Double
      case (Number(_, 1), Meld.Seq(_, 3)) => Edge
      case (Number(_, 7), Meld.Seq(_, 7)) => Edge
      case (Number(_, m), Meld.Seq(_, n)) if m == n + 1 => Closed
      case (Number(_, m), Meld.Seq(_, n)) if m == n || m == n + 2 => Sides
    }
  case object Sides extends Wait(0)
  case object Closed extends Wait(2)
  case object Edge extends Wait(2)
  case object Single extends Wait(2)
  case object Double extends Wait(0)
}
