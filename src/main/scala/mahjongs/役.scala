package mahjongs

import PartialFunction._

sealed abstract class 役 {
  type Check = PartialFunction[手牌, Int]
  val check: Check
}

case object 立直 extends 役 {
  lazy val check: Check = {
    case 手牌(_, Nil, _) => 1
  }
}

case object 二重立直 extends 役 {
  lazy val check: Check = {
    case 手牌(_, Nil, _) => 2
  }
}

case object 一発 extends 役 {
  lazy val check: Check = {
    case _ => 1
  }
}

case object 嶺上開花 extends 役 {
  lazy val check: Check = {
    case _ => 1
  }
}

case object 海底摸月 extends 役 {
  lazy val check: Check = {
    case _ => 1
  }
}

case object 河底撈魚 extends 役 {
  lazy val check: Check = {
    case _ => 1
  }
}

case object 槍槓 extends 役 {
  lazy val check: Check = {
    case _ => 1
  }
}

case class 役牌(tile: 字牌) extends 役 {
  lazy val check: Check = {
    case hand if hand.melds.exists(Set(刻子(tile), 槓子(tile))) => 1
  }
}

case object 門前清自摸和 extends 役 {
  lazy val check: Check = {
    case hand@手牌(_, Nil, _) if hand.situation.selfpick => 1
  }
}

case object 断么九 extends 役 {
  lazy val check: Check = {
    case hand if hand.melds.forall(_.tile.isInstanceOf[中張牌]) => 1
  }
}

case object 平和 extends 役 {
  lazy val check: Check = {
    case hand@手牌(concealed, Nil, 両面) if concealed.forall(meld => 門前.parse(meld)(hand.situation).isEmpty) => 1
  }
}

case object 一盃口 extends 役 {
  lazy val check: Check = {
    case 手牌(concealed, Nil, _) if concealed.groupBy(identity).exists {
      case (_: 順子, melds) => melds.size >= 2
      case _ => false
    } => 1
  }
}

case object 七対子 extends 役 with 符 {
  val point = 25
  lazy val check: Check = {
    case 手牌(concealed, Nil, _) if concealed.forall(_.isInstanceOf[対子]) => 2
  }
}

case object 混全帯么九 extends 役 {
  lazy val check: Check = {
    case hand@手牌(_, melded, _) if hand.melds.forall {
      case 順子(数牌(1)) => true 
      case 順子(数牌(7)) => true
      case 対子(_: 么九牌) => true
      case 刻子(_: 么九牌) => true
      case 槓子(_: 么九牌) => true
      case _ => false
    } => if (melded.isEmpty) 2 else 1
  }
}

case object 対々和 extends 役 {
  lazy val check: Check = {
    case hand if hand.melds.count(meld => meld.isInstanceOf[刻子] || meld.isInstanceOf[槓子]) == 4 => 2
  }
}

case object 一気通貫 extends 役 {
  lazy val check: Check = {
    case hand@手牌(_, melded, _) if 組.values.exists(suit => (0 until 9 by 3).forall(i => hand.melds.contains(順子(suit.values(i))))) =>
      if (melded.isEmpty) 2 else 1
  }
}

case object 三暗刻 extends 役 {
  lazy val check: Check = {
    case 手牌(concealed, _, _) if concealed.count(meld => meld.isInstanceOf[刻子] || meld.isInstanceOf[槓子]) == 3 => 2
  }
}

case object 三槓子 extends 役 {
  lazy val check: Check = {
    case hand if hand.melds.count(_.isInstanceOf[槓子]) == 3 => 2
  }
}

case object 三色同刻 extends 役 {
  lazy val check: Check = {
    case hand if (0 until 9).exists(i => 組.values.forall(suit => hand.melds.exists(Set(刻子(suit.values(i)), 槓子(suit.values(i)))))) => 2
  }
}

case object 三色同順 extends 役 {
  lazy val check: Check = {
    case hand@手牌(_, melded, _) if (0 until 9 by 3).exists(i => 組.values.forall(suit => hand.melds.contains(順子(suit.values(i))))) =>
      if (melded.isEmpty) 2 else 1
  }
}

case object 混老頭 extends 役 {
  lazy val check: Check = {
    case hand if hand.melds.forall(_.tile.isInstanceOf[么九牌]) => 2
  }
}

case object 小三元 extends 役 {
  lazy val check: Check = {
    case hand if 三元牌.values.permutations.exists(tiles => List[Set[面子]](Set(対子(tiles(0))), Set(刻子(tiles(1)), 槓子(tiles(1))), Set(刻子(tiles(2)), 槓子(tiles(1)))).forall(hand.melds.exists)) => 2
  }
}

case object 二盃口 extends 役 {
  lazy val check: Check = {
    case 手牌(concealed, Nil, _) if concealed.groupBy(identity).count {
      case (_: 順子, melds) => melds.size == 2
      case _ => false
    } == 2 => 3
  }
}

case object 純全帯么九 extends 役 {
  lazy val check: Check = {
    case hand@手牌(_, melded, _) if hand.melds.forall {
      case 順子(数牌(1)) => true 
      case 順子(数牌(7)) => true
      case 対子(_: 老頭牌) => true
      case 刻子(_: 老頭牌) => true
      case 槓子(_: 老頭牌) => true
      case _ => false
    } => if (melded.isEmpty) 3 else 2
  }
}

case object 混一色 extends 役 {
  lazy val check: Check = {
    case hand@手牌(_, melded, _) if 組.values.exists(suit => hand.melds.forall(meld => suit.values.contains(meld.tile) || 字牌.values.contains(meld.tile))) =>
      if (melded.isEmpty) 3 else 2
  }
}

case object 清一色 extends 役 {
  lazy val check: Check = {
    case hand@手牌(_, melded, _) if 組.values.exists(suit => hand.melds.forall(meld => suit.values.contains(meld.tile))) =>
      if (melded.isEmpty) 6 else 5
  }
}

object 役 {
  def values(prevailing: 風牌, player: 風牌): List[役] =
    List(役牌(prevailing), 役牌(player), 役牌(白), 役牌(發), 役牌(中), 門前清自摸和, 断么九, 平和, 一盃口, 七対子, 混全帯么九, 対々和, 一気通貫, 三暗刻, 三槓子, 三色同刻, 三色同順, 混老頭, 小三元, 二盃口, 純全帯么九, 混一色, 清一色)
  def dependencies: Map[役, 役] =
    Map(清一色 -> 混一色, 純全帯么九 -> 混全帯么九, 二盃口 -> 一盃口)
}
