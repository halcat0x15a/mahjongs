package mahjongs

import PartialFunction._

sealed abstract class 役(val value: Int, val decrease: Boolean) {
  def check(hand: 手牌): Boolean
}

object 役 {
  def values(prevailing: 風牌, player: 風牌): List[役] =
    List(役牌(prevailing), 役牌(player), 役牌(白), 役牌(發), 役牌(中), 門前清自摸和, 断么九, 平和, 一盃口, 七対子, 混全帯么九, 対々和, 一気通貫, 三暗刻, 三槓子, 三色同刻, 三色同順, 混老頭, 小三元, 二盃口, 純全帯么九, 混一色, 清一色)
  def dependencies: Map[役, 役] =
    Map(清一色 -> 混一色, 純全帯么九 -> 混全帯么九, 二盃口 -> 一盃口)
  case object 立直 extends 役(1, false) {
    def check(hand: 手牌) =
      hand.melded.isEmpty
  }
  case object 二重立直 extends 役(2, false) {
    def check(hand: 手牌) =
      hand.melded.isEmpty
  }
  case object 一発 extends 役(1, false) {
    def check(hand: 手牌) = true
  }
  case object 嶺上開花 extends 役(1, false) {
    def check(hand: 手牌) = true
  }
  case object 海底摸月 extends 役(1, false) {
    def check(hand: 手牌) = true
  }
  case object 河底撈魚 extends 役(1, false) {
    def check(hand: 手牌) = true
  }
  case object 槍槓 extends 役(1, false) {
    def check(hand: 手牌) = true
  }
  case class 役牌(tile: 字牌) extends 役(1, false) {
    def check(hand: 手牌) =
      hand.melds.exists(_.tile == tile)
  }
  case object 門前清自摸和 extends 役(1, false) {
    def check(hand: 手牌) =
      hand.situation.selfpick && hand.melded.isEmpty
  }
  case object 断么九 extends 役(1, false) {
    def check(hand: 手牌) =
      hand.melds.forall(_.tile.isInstanceOf[中張牌])
  }
  case object 平和 extends 役(1, false) {
    def check(hand: 手牌) = 
      hand.waiting == 両面 && hand.melded.isEmpty && hand.concealed.forall(meld => 符.parse(true, meld)(hand.situation).isEmpty)
  }
  case object 一盃口 extends 役(1, false) {
    def check(hand: 手牌) =
      hand.melded.isEmpty && hand.concealed.groupBy(identity).exists {
        case (_: 順子, melds) => melds.size >= 2
        case _ => false
      }
  }
  case object 七対子 extends 役(2, false) {
    def check(hand: 手牌) =
      hand.melded.isEmpty && hand.concealed.forall(_.isInstanceOf[対子])
  }
  case object 混全帯么九 extends 役(2, true) {
    def check(hand: 手牌) =
      hand.melds.forall {
        case 順子(数牌(1)) => true 
        case 順子(数牌(7)) => true
        case 対子(_: 么九牌) => true
        case 刻子(_: 么九牌) => true
        case 槓子(_: 么九牌) => true
        case _ => false
      }
  }
  case object 対々和 extends 役(2, false) {
    def check(hand: 手牌) =
      hand.melds.count(meld => meld.isInstanceOf[刻子] || meld.isInstanceOf[槓子]) == 4
  }
  case object 一気通貫 extends 役(2, true) {
    def check(hand: 手牌) =
      組.values.exists(suit => (0 until 9 by 3).forall(i => hand.melds.contains(順子(suit.values(i)))))
  }
  case object 三暗刻 extends 役(2, false) {
    def check(hand: 手牌) =
      hand.concealed.count(meld => meld.isInstanceOf[刻子] || meld.isInstanceOf[槓子]) == 3
  }
  case object 三槓子 extends 役(2, false) {
    def check(hand: 手牌) =
      hand.melds.count(_.isInstanceOf[槓子]) == 3
  }
  case object 三色同刻 extends 役(2, false) {
    def check(hand: 手牌) =
      (0 until 9).exists(i => 組.values.forall(suit => hand.melds.exists(Set(刻子(suit.values(i)), 槓子(suit.values(i))))))
  }
  case object 三色同順 extends 役(2, true) {
    def check(hand: 手牌) =
      (0 until 9 by 3).exists(i => 組.values.forall(suit => hand.melds.contains(順子(suit.values(i)))))
  }
  case object 混老頭 extends 役(2, false) {
    def check(hand: 手牌) =
      hand.melds.forall(_.tile.isInstanceOf[么九牌])
  }
  case object 小三元 extends 役(2, false) {
    def check(hand: 手牌) =
      三元牌.values.permutations.exists(tiles => List[Set[面子]](Set(対子(tiles(0))), Set(刻子(tiles(1)), 槓子(tiles(1))), Set(刻子(tiles(2)), 槓子(tiles(1)))).forall(hand.melds.exists))
  }
  case object 二盃口 extends 役(3, false) {
    def check(hand: 手牌) =
      hand.melded.isEmpty && hand.concealed.groupBy(identity).count {
        case (_: 順子, melds) => melds.size == 2
        case _ => false
      } == 2
  }
  case object 純全帯么九 extends 役(3, true) {
    def check(hand: 手牌) =
      hand.melds.forall {
        case 順子(数牌(1)) => true 
        case 順子(数牌(7)) => true
        case 対子(_: 老頭牌) => true
        case 刻子(_: 老頭牌) => true
        case 槓子(_: 老頭牌) => true
        case _ => false
      }
  }
  case object 混一色 extends 役(3, true) {
    def check(hand: 手牌) =
      組.values.exists(suit => hand.melds.forall(meld => suit.values.contains(meld.tile) || 字牌.values.contains(meld.tile)))
  }
  case object 清一色 extends 役(6, true) {
    def check(hand: 手牌) =
      組.values.exists(suit => hand.melds.forall(meld => suit.values.contains(meld.tile)))
  }
}
