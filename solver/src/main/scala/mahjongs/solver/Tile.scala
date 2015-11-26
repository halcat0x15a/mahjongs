package mahjongs.solver

import scala.collection.breakOut

sealed trait Suit

case object Wan extends Suit

case object Pin extends Suit

case object Sou extends Suit

sealed trait Tile {

  lazy val index: Int = Tile.values.indexOf(this)

  lazy val isTerminal: Boolean =
    this match {
      case Num(_, number) => number == 1 || number == 9
      case _ => false
    }

  lazy val isNumber: Boolean = isInstanceOf[Num]

  lazy val isHonor: Boolean = !isNumber

  lazy val isOrphan: Boolean = isTerminal || isHonor

  lazy val isDragon: Boolean = this == White || this == Green || this == Red

}

case class Num(suit: Suit, number: Int) extends Tile

case object East extends Tile

case object South extends Tile

case object West extends Tile

case object North extends Tile

case object White extends Tile

case object Green extends Tile

case object Red extends Tile

object Tile {

  val suit: Vector[Suit] = Vector(Wan, Pin, Sou)

  val wan: Vector[Num] = (1 to 9).map(Num(Wan, _))(breakOut)

  val pin: Vector[Num] = (1 to 9).map(Num(Pin, _))(breakOut)

  val sou: Vector[Num] = (1 to 9).map(Num(Sou, _))(breakOut)

  val num: Vector[Num] = wan ++ pin ++ sou

  val wind: Vector[Tile] = Vector(East, South, West, North)

  val dragon: Vector[Tile] = Vector(White, Green, Red)

  val values: Vector[Tile] = num ++ wind ++ dragon

}
