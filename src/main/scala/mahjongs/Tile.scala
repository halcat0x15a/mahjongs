package mahjongs

sealed trait Tile {
  def isTerminal =
    this match {
      case Number(_, n) => n == 1 || n == 9
      case _ => false
    }
  def isHonor = !isInstanceOf[Number]
  def unicode: String
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

case class Number(suit: Suit, value: Int) extends Tile {
  def unicode = suit match {
    case Character => value match {
      case 1 => "\ud83c\udc07"
      case 2 => "\ud83c\udc08"
      case 3 => "\ud83c\udc09"
      case 4 => "\ud83c\udc0a"
      case 5 => "\ud83c\udc0b"
      case 6 => "\ud83c\udc0c"
      case 7 => "\ud83c\udc0d"
      case 8 => "\ud83c\udc0e"
      case 9 => "\ud83c\udc0f"
    }
    case Circle => value match {
      case 1 => "\ud83c\udc19"
      case 2 => "\ud83c\udc1a"
      case 3 => "\ud83c\udc1b"
      case 4 => "\ud83c\udc1c"
      case 5 => "\ud83c\udc1d"
      case 6 => "\ud83c\udc1e"
      case 7 => "\ud83c\udc1f"
      case 8 => "\ud83c\udc20"
      case 9 => "\ud83c\udc21"
    }
    case Bamboo => value match {
      case 1 => "\ud83c\udc10"
      case 2 => "\ud83c\udc11"
      case 3 => "\ud83c\udc12"
      case 4 => "\ud83c\udc13"
      case 5 => "\ud83c\udc14"
      case 6 => "\ud83c\udc15"
      case 7 => "\ud83c\udc16"
      case 8 => "\ud83c\udc17"
      case 9 => "\ud83c\udc18"
    }
  }
}

sealed trait Wind extends Tile {
  def unicode = this match {
    case East => "\ud83c\udc00"
    case South => "\ud83c\udc01"
    case West => "\ud83c\udc02"
    case North => "\ud83c\udc03"
  }
}

case object East extends Wind

case object South extends Wind

case object West extends Wind

case object North extends Wind

object Wind {
  def values = Vector(East, South, West, North)
}

sealed trait Dragon extends Tile {
  def unicode = this match {
    case White => "\ud83c\udc06"
    case Green => "\ud83c\udc05"
    case Red => "\ud83c\udc04"
  }
}

case object White extends Dragon

case object Green extends Dragon

case object Red extends Dragon

object Dragon {
  def values = Vector(White, Green, Red)
}
