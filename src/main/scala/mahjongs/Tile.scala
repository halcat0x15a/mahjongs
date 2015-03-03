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
  def values: IndexedSeq[Tile] =
    Character.values ++ Circle.values ++ Bamboo.values ++ Wind.values ++ Dragon.values
  implicit def ordering: Ordering[Tile] =
    new Ordering[Tile] {
      def compare(x: Tile, y: Tile) =
        values.indexOf(x) compareTo values.indexOf(y)
    }
}

sealed trait Suit {
  def apply(value: Int) = Number(this, value)
  def values = 1 to 9 map apply
}

case object Character extends Suit

case object Circle extends Suit

case object Bamboo extends Suit

object Suit {
  def values = Vector(Character, Circle, Bamboo)
}

case class Number(suit: Suit, value: Int) extends Tile

sealed trait Wind extends Tile

case object East extends Wind

case object South extends Wind

case object West extends Wind

case object North extends Wind

object Wind {
  def values = Vector(East, South, West, North)
}

sealed trait Dragon extends Tile

case object White extends Dragon

case object Green extends Dragon

case object Red extends Dragon

object Dragon {
  def values = Vector(White, Green, Red)
}
