package mahjongs.solver

sealed abstract class Meld {

  def tile: Tile

  def tiles: List[Tile]

  def isClosed: Boolean

  def contains(suit: Suit): Boolean =
    tile match {
      case Num(`suit`, _) => true
      case _ => false
    }

  val isSeq: Boolean = isInstanceOf[Shuntsu]

  val isQuad: Boolean = isInstanceOf[Kantsu]

  val isTriplet: Boolean = isInstanceOf[Kotsu] || isQuad

  val isPair: Boolean = isInstanceOf[Toitsu]

  lazy val isTerminal: Boolean =
    this match {
      case Shuntsu(Num(_, 1 | 7), _) => true
      case _ => tile.isTerminal
    }

}

case class Toitsu(tile: Tile) extends Meld {

  val isClosed: Boolean = true

  val tiles: List[Tile] = List.fill(2)(tile)

}

case class Shuntsu(tile: Num, isClosed: Boolean) extends Meld {

  val tiles: List[Tile] = (0 until 3).map(i => tile.copy(number = tile.number + i))(collection.breakOut)

}

case class Kotsu(tile: Tile, isClosed: Boolean) extends Meld {

  val tiles: List[Tile] = List.fill(3)(tile)

}

case class Kantsu(tile: Tile, isClosed: Boolean) extends Meld {

  val tiles: List[Tile] = List.fill(4)(tile)

}

object Meld {

  def set(tile: Tile, size: Int): Option[Meld] =
    size match {
      case 2 => Some(Toitsu(tile))
      case 3 => Some(Kotsu(tile, true))
      case 4 => Some(Kantsu(tile, true))
      case _ => None
    }

  def parse(tiles: List[Tile], isClosed: Boolean): Option[Meld] =
    tiles.sortBy(_.index) match {
      case a :: b :: c :: Nil if a == b && b == c => Some(Kotsu(a, isClosed))
      case a :: b :: c :: d :: Nil if a == b && b == c && c == d => Some(Kantsu(a, isClosed))
      case (tile@Num(a, x)) :: Num(b, y) :: Num(c, z) :: Nil if a == b && b == c && x == y - 1 && y == z - 1 => Some(Shuntsu(tile, isClosed))
      case _ => None
    }
 
  def fu(meld: Meld, situation: Situation): Int =
    meld match {
      case Toitsu(tile) if tile == situation.seatWind && tile == situation.roundWind => 4
      case Toitsu(tile) if situation.isHonor(tile) => 2
      case Toitsu(_) => 0
      case Shuntsu(_, _) => 0
      case Kotsu(tile, true) if tile.isOrphan => 8
      case Kotsu(tile, isClosed) if isClosed || tile.isOrphan => 4
      case Kotsu(_, _) => 2
      case Kantsu(tile, true) if tile.isOrphan => 32
      case Kantsu(tile, isClosed) if isClosed || tile.isOrphan => 16
      case Kantsu(_, _) => 8
    }

}
