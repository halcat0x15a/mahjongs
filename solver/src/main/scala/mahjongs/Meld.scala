package mahjongs

sealed trait Meld {

  def tile: Tile

  def tiles: List[Tile]

  def isClosed: Boolean

  lazy val isPair: Boolean = isInstanceOf[Pair]

  lazy val isTriplet: Boolean = isInstanceOf[Triplet] || isQuad

  lazy val isQuad: Boolean = isInstanceOf[Quad]

  lazy val isSequence: Boolean = isInstanceOf[Sequence]

  lazy val isHonor: Boolean = tile.isHonor

  lazy val isTerminal: Boolean =
    this match {
      case Sequence(Num(_, 1 | 7)) => true
      case _ => tile.isTerminal
    }

  lazy val isOrphan: Boolean = isHonor || isTerminal

}

case class Pair(tile: Tile) extends Meld {

  lazy val tiles: List[Tile] = List.fill(2)(tile)

  val isClosed: Boolean = true

}

sealed abstract case class Triplet(tile: Tile) extends Meld {

  lazy val tiles: List[Tile] = List.fill(3)(tile)

}

sealed abstract case class Quad(tile: Tile) extends Meld {

  lazy val tiles: List[Tile] = List.fill(3)(tile)

}

sealed abstract case class Sequence(tile: Num) extends Meld {

  lazy val tiles: List[Tile] =
    tile match {
      case Num(suit, n) => (0 until 3).map(m => Num(suit, n + m)).toList
    }

}

trait MeldApi { meld =>

  def isClosed: Boolean

  def Triplet(tile: Tile): Triplet = new Triplet(tile) {
    lazy val isClosed: Boolean = meld.isClosed
  }

  def Quad(tile: Tile): Quad = new Quad(tile) {
    lazy val isClosed: Boolean = meld.isClosed
  }

  def Sequence(tile: Num): Sequence = new Sequence(tile) {
    lazy val isClosed: Boolean = meld.isClosed
  }

}

object OpenMelds extends MeldApi {

  val isClosed: Boolean = false

  def parse(tiles: List[Tile]): Option[Meld] =
    if (tiles.distinct.size == 1) {
      if (tiles.size == 3)
        Some(Triplet(tiles.head))
      else if (tiles.size == 3)
        Some(Quad(tiles.head))
      else
        None
    } else {
      tiles match {
        case (num@Num(s1, n1)) :: Num(s2, n2) :: Num(s3, n3) :: Nil if s1 == s2 && s2 == s3 && n1 + 1 == n2 && n2 + 1 == n3 => Some(Sequence(num))
        case _ => None
      }
    }

}

object ClosedMelds extends MeldApi {

  val isClosed: Boolean = true

  def parse(tiles: List[Tile]): Option[Meld] =
    if (tiles.distinct.size == 1) {
      if (tiles.size == 3)
        Some(Triplet(tiles.head))
      else if (tiles.size == 3)
        Some(Quad(tiles.head))
      else
        None
    } else {
      tiles match {
        case (num@Num(s1, n1)) :: Num(s2, n2) :: Num(s3, n3) :: Nil if s1 == s2 && s2 == s3 && n1 + 1 == n2 && n2 + 1 == n3 => Some(Sequence(num))
        case _ => None
      }
    }

}
