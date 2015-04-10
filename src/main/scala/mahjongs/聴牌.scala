package mahjongs

import PartialFunction._

sealed trait 聴牌 extends 符

object 聴牌 {
  def values = List(両面, 嵌張, 辺張, 単騎, 双碰)
  def parse(tile: 牌, meld: 面子): Option[聴牌] = { import 面子._
    condOpt(tile -> meld) {
      case (_, 対子(`tile`)) => 単騎
      case (_, 刻子(`tile`)) => 双碰
      case (数牌(n), 順子(数牌(m))) if n == 3 && m == 1 || n == 7 && m == 7=> 辺張
      case (数牌(n), 順子(数牌(m))) if n == m + 1 => 嵌張
      case (数牌(n), 順子(数牌(m))) if n == m || n == m + 2 => 両面
    }
  }
  case object 両面 extends 聴牌 { val value = 0 }
  case object 嵌張 extends 聴牌 { val value = 2 }
  case object 辺張 extends 聴牌 { val value = 2 }
  case object 単騎 extends 聴牌 { val value = 2 }
  case object 双碰 extends 聴牌 { val value = 0 }
}
