package mahjongs

sealed abstract class 面子(val tiles: List[牌]) {
  val tile: 牌
}

case class 対子(tile: 牌) extends 面子(List.fill(2)(tile))

case class 刻子(tile: 牌) extends 面子(List.fill(3)(tile))

case class 槓子(tile: 牌) extends 面子(List.fill(4)(tile))

case class 順子(tile: 数牌) extends 面子(tile.sequence)

object 面子 {
  def parse(tiles: Seq[牌]): Option[面子] =
    tiles match {
      case (tile: 数牌) +: _ => List(対子(tile), 刻子(tile), 槓子(tile), 順子(tile)).find(_.tiles == tiles)
      case tile +: _ => List(対子(tile), 刻子(tile), 槓子(tile)).find(_.tiles == tiles)
      case _ => None
    }
}
