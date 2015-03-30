package mahjongs

import PartialFunction._

sealed trait 面子 {
  val tile: 牌
  val tiles: List[牌]
}

case class 対子(tile: 牌) extends 面子 {
  val tiles = List.fill(2)(tile)
}

case class 刻子(tile: 牌) extends 面子 {
  val tiles = List.fill(3)(tile)
}

case class 槓子(tile: 牌) extends 面子 {
  val tiles = List.fill(4)(tile)
}

case class 順子(tile: 数牌) extends 面子 {
  val tiles = tile.sequence
}

object 面子 {
  def parse(tiles: scala.Seq[牌]): Option[面子] =
    condOpt(tiles) {
      case numbers@(number: 数牌) +: _ if numbers == number.sequence => 順子(number)
      case tiles@Seq(tile, _) if tiles.forall(_ == tile) => 対子(tile)
      case tiles@Seq(tile, _, _) if tiles.forall(_ == tile) => 刻子(tile)
      case tiles@Seq(tile, _, _, _) if tiles.forall(_ == tile) => 槓子(tile)
    }
}
