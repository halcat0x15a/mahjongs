package mahjongs

sealed trait Wait {
  import Wait._
  def fu: Int =
    this match {
      case Closed | Edge | Single => 2
      case Sides | Double => 0
    }
}

object Wait {
  def find(tile: Tile, hand: Seq[Meld]) =
    hand.find(_.tiles.contains(tile)).collect {
      case Set(tiles) if tiles == 2 => Single
      case Set(tiles) if tiles == 3 => Double
      case Run(Number(_, n) +: _) => tile match {
        case Number(_, m) if (n == 1 && m == 3) || (n == 7 && m == 7) => Edge
        case Number(_, m) if m == n + 1 => Closed
        case _ => Sides
      }
    }
  case object Sides extends Wait
  case object Closed extends Wait
  case object Edge extends Wait
  case object Single extends Wait
  case object Double extends Wait
}
