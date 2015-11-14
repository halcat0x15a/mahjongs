package mahjongs

sealed trait Tile {

  lazy val isNumber: Boolean = isInstanceOf[Num]

  lazy val isWind: Boolean = isInstanceOf[Wind]

  lazy val isDragon: Boolean = isInstanceOf[Dragon]

  lazy val isHonor: Boolean = isWind || isDragon

  lazy val isTerminal: Boolean =
    this match {
      case Num(_, 1) => true
      case Num(_, 9) => true
      case _ => false
    }

  lazy val isOrphan: Boolean = isHonor || isTerminal

  lazy val unicode: String =
    this match {
      case Num(Wan, n) => s"\ud83c${(n + 0xdc06).toChar}"
      case Num(Pin, n) => s"\ud83c${(n + 0xdc18).toChar}"
      case Num(Sou, n) => s"\ud83c${(n + 0xdc0f).toChar}"
      case East => "\ud83c\udc00"
      case South => "\ud83c\udc01"
      case West => "\ud83c\udc02"
      case North => "\ud83c\udc03"
      case White => "\ud83c\udc06"
      case Green => "\ud83c\udc05"
      case Red => "\ud83c\udc04"
    }

}

object Tile {

  val values: Vector[Tile] = Num.values ++ Wind.values ++ Dragon.values

  implicit val ordering: Ordering[Tile] =
    new Ordering[Tile] {
      def compare(x: Tile, y: Tile): Int =
        values.indexOf(x) compareTo values.indexOf(y)
    }

}

sealed trait Suit

object Suit {

  val values: Vector[Suit] = Vector(Wan, Pin, Sou)

}

case object Wan extends Suit

case object Pin extends Suit

case object Sou extends Suit

case class Num(suit: Suit, n: Int) extends Tile

object Num {

  val values: Vector[Num] =
    for {
      suit <- Suit.values
      n <- 1 to 9
    } yield Num(suit, n)

}

sealed trait Wind extends Tile

object Wind {

  val values: Vector[Wind] = Vector(East, South, West, North)

}

case object East extends Wind

case object South extends Wind

case object West extends Wind

case object North extends Wind

sealed trait Dragon extends Tile

object Dragon {

  val values: Vector[Dragon] = Vector(White, Green, Red)

}

case object White extends Dragon

case object Green extends Dragon

case object Red extends Dragon
