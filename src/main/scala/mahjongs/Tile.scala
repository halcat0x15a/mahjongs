package mahjongs

sealed trait Tile {
  def isTerminal =
    this match {
      case Number(_, n) => n == 1 || n == 9
      case _ => false
    }
  def isHonor = !isInstanceOf[Number]
}

object Tile {
  def indexOf(tile: Tile) =
    tile match {
      case _: Number => 0
      case _: Wind => 1
      case _: Dragon => 2
    }
  implicit def ordering =
    new Ordering[Tile] {
      def compare(x: Tile, y: Tile) =
        (x, y) match {
          case (x: Number, y: Number) => Ordering[Number].compare(x, y)
          case (x: Wind, y: Wind) => Ordering[Wind].compare(x, y)
          case (x: Dragon, y: Dragon) => Ordering[Dragon].compare(x, y)
          case _ => indexOf(x) compareTo indexOf(y)
        }
    }
}

sealed trait Suit

case object Character extends Suit

case object Circle extends Suit

case object Bamboo extends Suit

object Suit extends Enum[Suit] {
  def values = Seq(Character, Circle, Bamboo)
}

case class Number(suit: Suit, value: Int) extends Tile

object Number {
  implicit def ordering: Ordering[Number] =
    Ordering.by((n: Number) => n.suit -> n.value)
}

sealed trait Wind extends Tile

case object East extends Wind

case object South extends Wind

case object West extends Wind

case object North extends Wind

object Wind extends Enum[Wind] {
  def values = Seq(East, South, West, North)
}

sealed trait Dragon extends Tile

case object Red extends Dragon

case object Green extends Dragon

case object White extends Dragon

object Dragon extends Enum[Dragon] {
  def values = Seq(Red, Green, White)
}
