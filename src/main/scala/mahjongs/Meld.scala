package mahjongs

import PartialFunction._

sealed trait Meld {
  def tiles: Seq[Tile]
  def contains(suit: Suit): Boolean =
    this match {
      case Meld.Set(Number(`suit`, _), _) => true
      case Meld.Seq(`suit`, _) => true
      case _ => false
    }
  def isTerminal: Boolean =
    this match {
      case Meld.Set(tile, _) => tile.isTerminal
      case _ => false
    }
  def isHonor: Boolean =
    this match {
      case Meld.Set(tile, _) => tile.isHonor
      case _ => false
    }
  def isOutside: Boolean =
    this match {
      case Meld.Set(tile, _) => tile.isTerminal
      case Meld.Seq(_, n) => n == 1 || n == 7
    }
}

object Meld {
  def parse(tiles: scala.Seq[Tile]): Option[Meld] =
    condOpt(tiles) {
      case numbers@Number(suit, start) +: _ if numbers == (start until start + 3 map suit.apply) => Seq(suit, start)
      case tiles@scala.Seq(tile, _) if tiles.forall(_ == tile) => Pair(tile)
      case tiles@scala.Seq(tile, _, _) if tiles.forall(_ == tile) => Triplet(tile)
      case tiles@scala.Seq(tile, _, _, _) if tiles.forall(_ == tile) => Quad(tile)
    }
  sealed trait Set extends Meld {
    val tile: Tile
    val size: Int
    lazy val tiles = List.fill(size)(tile)
  }
  case class Pair(tile: Tile) extends Set { val size = 2 }
  case class Triplet(tile: Tile) extends Set { val size = 3 }
  case class Quad(tile: Tile) extends Set { val size = 4 }
  case class Seq(suit: Suit, start: Int) extends Meld {
    lazy val tiles = start until start + 3 map suit.apply
  }
  object Set {
    def apply(tile: Tile, size: Int): Set =
      size match {
        case 2 => Pair(tile)
        case 3 => Triplet(tile)
        case 4 => Quad(tile)
      }
    def unapply(meld: Meld): Option[(Tile, Int)] =
      condOpt(meld) {
        case Pair(tile) => tile -> 2
        case Triplet(tile) => tile -> 3
        case Quad(tile) => tile -> 4
      }
  }
}
