package mahjongs

import PartialFunction._

sealed trait Wait

object Wait {
  def find(winning: Int, hand: Seq[Meld]): Option[Wait] =
    condOpt((for (meld <- hand; tile <- meld.tiles) yield tile -> meld).apply(winning)) {
      case (_, Meld.Set(_, 2)) => Single
      case (_, Meld.Set(_, 3)) => Double
      case (Number(_, 1), Meld.Seq(_, 3)) => Edge
      case (Number(_, 7), Meld.Seq(_, 7)) => Edge
      case (Number(_, m), Meld.Seq(_, n)) if m == n + 1 => Closed
      case (Number(_, m), Meld.Seq(_, n)) if m == n || m == n + 2 => Sides
    }
  case object Sides extends Wait
  case object Closed extends Wait
  case object Edge extends Wait
  case object Single extends Wait
  case object Double extends Wait
}
