package mahjongs

sealed trait Wait

object Wait {

  val values = Vector(単騎, 双碰, 辺張, 嵌張, 両面)

  def waiting(tile: Tile, meld: Meld): Option[Wait] =
    (tile, meld) match {
      case (_, Pair(`tile`)) => Some(単騎)
      case (_, Triplet(`tile`)) => Some(双碰)
      case (Num(_, n), Sequence(Num(_, m))) if n == 3 && m == 1 || n == 7 && m == 7=> Some(辺張)
      case (Num(_, n), Sequence(Num(_, m))) if n == m + 1 => Some(嵌張)
      case (Num(_, n), Sequence(Num(_, m))) if n == m || n == m + 2 => Some(両面)
      case _ => None
    }

  case object 単騎 extends Wait

  case object 双碰 extends Wait

  case object 辺張 extends Wait

  case object 嵌張 extends Wait

  case object 両面 extends Wait

}
