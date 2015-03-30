package mahjongs

import PartialFunction._

trait 符 {
  val value: Int
}

object 符 {
  def parse(concealed: Boolean, meld: 面子)(implicit situation: 状況): Option[符] = { import 面子._
    condOpt(meld) {
      case 対子(tile: 三元牌) => 雀頭(tile)
      case 対子(tile) if tile == situation.prevailing || tile == situation.player => 雀頭(tile)
      case 刻子(tile) => if (concealed) 暗刻子(tile) else 明刻子(tile)
      case 槓子(tile) => if (concealed) 暗槓子(tile) else 明槓子(tile)
    }
  }
  case object 副底 extends 符 { val value = 20 }
  case object 門前加符 extends 符 { val value = 10 }
  case object 自摸符 extends 符 { val value = 2 }
  case class 雀頭(tile: 牌) extends 符 { val value = 2 }
  case class 明刻子(tile: 牌) extends 符 {
    val value = tile match {
      case _: 么九牌 => 4
      case _ => 2
    }
  }
  case class 暗刻子(tile: 牌) extends 符 {
    val value = tile match {
      case _: 么九牌 => 8
      case _ => 4
    }
  }
  case class 明槓子(tile: 牌) extends 符 {
    val value = tile match {
      case _: 么九牌 => 16
      case _ => 8
    }
  }
  case class 暗槓子(tile: 牌) extends 符 {
    val value = tile match {
      case _: 么九牌 => 32
      case _ => 16
    }
  }
  case object 自摸平和 extends 符 { val value = 20 }
  case object 七対子 extends 符 { val value = 25 }
  case object 栄平和 extends 符 { val value = 30 }
}
