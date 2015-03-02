package mahjongs

import PartialFunction._

sealed trait Meld extends Any {
  def tiles: Seq[Tile]
  def fu: Int =
    this match {
      case Meld.Set(tile, 3) =>
        if (tile.isTerminal || tile.isHonor) 8 else 4
      case Meld.Set(tile, 4) =>
        if (tile.isTerminal || tile.isHonor) 32 else 16
      case Meld.Set(tile, 4) if tile.isTerminal || tile.isHonor => 2
      case _ => 0
    }
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
  sealed trait Set extends Any with Meld {
    def tile: Tile
    def size: Int
    def tiles = List.fill(size)(tile)
  }
  case class Pair(tile: Tile) extends AnyVal with Set {
    def size = 2
  }
  case class Triplet(tile: Tile) extends AnyVal with Set {
    def size = 3
  }
  case class Quad(tile: Tile) extends AnyVal with Set {
    def size = 4
  }
  case class Seq(suit: Suit, start: Int) extends Meld {
    def tiles = start to start + 3 map suit.apply
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
