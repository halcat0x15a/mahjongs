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
    hand.find(_.tiles.contains(tile)).map(tile -> _).collect {
      case (_, Set(tiles)) if tiles.size == 2 => Single
      case (_, Set(tiles)) if tiles.size == 3 => Double
      case (Number(_, m), Run(Number(_, n) +: _)) if n == 1 && m == 3 || n == 7 && m == 7 => Edge
      case (Number(_, m), Run(Number(_, n) +: _)) if m == n + 1 => Closed
      case (Number(_, m), Run(Number(_, n) +: _)) if m == n || m == n + 2 => Sides
    }
  case object Sides extends Wait
  case object Closed extends Wait
  case object Edge extends Wait
  case object Single extends Wait
  case object Double extends Wait
}
