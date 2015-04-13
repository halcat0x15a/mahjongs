package mahjongs

import PartialFunction._

trait 符 {
  def point: Int
}

case object 副底 extends 符 {
  def point = 20
}

case object 門前加符 extends 符 {
  def point = 10
}

case object 自摸符 extends 符 {
  def point = 2
}

sealed abstract class 門前 extends 符
object 門前 {
  def parse(meld: 面子)(implicit situation: 状況): Option[門前] =
    condOpt(meld) {
      case 対子(tile: 三元牌) => 雀頭(tile)
      case 対子(tile) if tile == situation.prevailing || tile == situation.player => 雀頭(tile)
      case 刻子(tile) => 暗刻子(tile)
      case 槓子(tile) => 暗槓子(tile)
    }
}

case class 雀頭(tile: 牌)(implicit situation: 状況) extends 門前 {
  def point = if (tile == situation.prevailing && tile == situation.player) 4 else 2
}

case class 暗刻子(tile: 牌) extends 門前 {
  def point = tile match {
    case _: 么九牌 => 8
    case _ => 4
  }
}

case class 暗槓子(tile: 牌) extends 門前 {
  def point = tile match {
    case _: 么九牌 => 32
    case _ => 16
  }
}

sealed abstract class 副露 extends 符
object 副露 {
  def parse(meld: 面子): Option[副露] =
    condOpt(meld) {
      case 刻子(tile) => 明刻子(tile)
      case 槓子(tile) => 明槓子(tile)
    }
}

case class 明刻子(tile: 牌) extends 副露 {
  def point = tile match {
    case _: 么九牌 => 4
    case _ => 2
  }
}

case class 明槓子(tile: 牌) extends 副露 {
  def point = tile match {
    case _: 么九牌 => 16
    case _ => 8
  }
}

case object 自摸平和 extends 符 {
  def point = 20
}

case object 栄平和 extends 符 {
  def point = 30
}
