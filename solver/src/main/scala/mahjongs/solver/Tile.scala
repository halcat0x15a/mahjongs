package mahjongs.solver

import scala.collection.breakOut

sealed abstract class Suit

case object Wan extends Suit

case object Pin extends Suit

case object Sou extends Suit

object Suit {

  val values: Vector[Suit] = Vector(Wan, Pin, Sou)

}

sealed abstract class Tile {

  lazy val index: Int = Tile.values.indexOf(this)

  val isNumber: Boolean = isInstanceOf[Num]

  val isWind: Boolean = isInstanceOf[Wind]

  val isDragon: Boolean = isInstanceOf[Dragon]

  val isHonor: Boolean = isWind || isDragon

  val isTerminal: Boolean =
    this match {
      case Num(_, 1 | 9) => true
      case _ => false
    }

  val isOrphan: Boolean = isTerminal || isHonor

}

case class Num(suit: Suit, number: Int) extends Tile

object Num {

  val values: Vector[Num] =
    for {
      suit <- Suit.values
      n <- 1 to 9
    } yield Num(suit, n)

}

sealed abstract class Wind extends Tile

case object East extends Wind

case object South extends Wind

case object West extends Wind

case object North extends Wind

object Wind {

  val values: Vector[Wind] = Vector(East, South, West, North)

}

sealed abstract class Dragon extends Tile

case object White extends Dragon

case object Green extends Dragon

case object Red extends Dragon

object Dragon {

  val values: Vector[Dragon] = Vector(White, Green, Red)

}

object Tile {

  val values: Vector[Tile] = Num.values ++ Wind.values ++ Dragon.values

}
