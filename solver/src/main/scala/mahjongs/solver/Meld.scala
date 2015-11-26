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

  val isPair: Boolean = isInstanceOf[Toitsu]

  val isSeq: Boolean = isInstanceOf[Shuntsu]

  val isTriplet: Boolean = isInstanceOf[Kotsu]

  val isQuad: Boolean = isInstanceOf[Kantsu]

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
    if (tiles.distinct.size == 1) {
      if (tiles.size == 3)
        Some(Kotsu(tiles.head, isClosed))
      else if (tiles.size == 4)
        Some(Kantsu(tiles.head, isClosed))
      else
        None
    } else if (tiles.forall(_.isNumber)) {
      tiles.collect { case num@Num(_, _) => num }.sortBy(_.number).headOption.map(Shuntsu(_, isClosed))
    } else {
      None
    }

 
  def fu(meld: Meld, seatWind: Tile, prevailingWind: Tile): Int =
    meld match {
      case Toitsu(tile) if tile == seatWind && tile == prevailingWind => 4
      case Toitsu(tile) if tile == seatWind || tile == prevailingWind || Tile.dragon.contains(tile) => 2
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
