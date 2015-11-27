package mahjongs.solver

sealed abstract class Yaku(val name: String) extends PartialFunction[Hand, Int]

case object Dora extends Yaku("ドラ") {

  def apply(hand: Hand): Int =
    hand.situation.dora

  def isDefinedAt(hand: Hand): Boolean =
    hand.situation.dora > 0

}

case object Yakuhai extends Yaku("役牌") {

  def apply(hand: Hand): Int =
    hand.melds.filter(_.isTriplet).collect {
      case meld if hand.situation.isDoubleWind(meld.tile) => 2
      case meld if hand.situation.isHonor(meld.tile) => 1
    }.sum

  def isDefinedAt(hand: Hand): Boolean =
    hand.isClosed && hand.situation.isSelfDrawn

}

case object Menzenchintsumohou extends Yaku("門前清自摸和") {

  def apply(hand: Hand): Int = 1

  def isDefinedAt(hand: Hand): Boolean =
    hand.isClosed && hand.situation.isSelfDrawn

}

case object Pinfu extends Yaku("平和") {

  def apply(hand: Hand): Int = 1

  def isDefinedAt(hand: Hand): Boolean =
    hand.isClosed && hand.isNoPoints

}

case object Iipeikou extends Yaku("一盃口") {

  def apply(hand: Hand): Int = 1

  def isDefinedAt(hand: Hand): Boolean =
    hand.isClosed && hand.grouped.exists {
      case (meld, n) => meld.isSeq && n >= 2
    } && !Ryanpeikou.isDefinedAt(hand)

}

case object Tanyaochuu extends Yaku("断么九") {

  def apply(hand: Hand): Int = 1

  def isDefinedAt(hand: Hand): Boolean =
    hand.isClosed && hand.tiles.forall(!_.isOrphan)

}

case object Sanshokudoujun extends Yaku("三色同順") {

  def apply(hand: Hand): Int = if (hand.isClosed) 2 else 1

  def isDefinedAt(hand: Hand): Boolean =
    (1 to 7).exists(n => Suit.values.forall(suit => hand.melds.exists(_.tile == Num(suit, n))))

}

case object Ikkitsuukan extends Yaku("一気通貫") {

  def apply(hand: Hand): Int = if (hand.isClosed) 2 else 1

  def isDefinedAt(hand: Hand): Boolean =
    Suit.values.exists(suit => (1 to 9 by 3).forall(n => hand.melds.exists(meld => meld.isSeq && meld.tile == Num(suit, n))))

}

case object Chantaiyao extends Yaku("混全帯么九") {

  def apply(hand: Hand): Int = if (hand.isClosed) 2 else 1

  def isDefinedAt(hand: Hand): Boolean =
    hand.melds.forall(meld => meld.isTerminal || meld.tile.isOrphan) && !Junchantaiyao.isDefinedAt(hand)

}

case object Chiitoitsu extends Yaku("七対子") {

  def apply(hand: Hand): Int = 2

  def isDefinedAt(hand: Hand): Boolean =
    hand.isSevenPairs

}

case object Toitoihou extends Yaku("対々和") {

  def apply(hand: Hand): Int = 2

  def isDefinedAt(hand: Hand): Boolean =
    hand.melds.count(_.isTriplet) == 4

}

case object Sanankou extends Yaku("三暗刻") {

  def apply(hand: Hand): Int = 2

  def isDefinedAt(hand: Hand): Boolean =
    hand.closedMelds.count(_.isTriplet) >= 3

}

case object Sanshokudoukou extends Yaku("三色同刻") {

  def apply(hand: Hand): Int = 2

  def isDefinedAt(hand: Hand): Boolean =
    (1 to 9).exists(n => Suit.values.forall(suit => hand.melds.exists(_.tile == Num(suit, n))))

}

case object Honroutou extends Yaku("混老頭") {

  def apply(hand: Hand): Int = 2

  def isDefinedAt(hand: Hand): Boolean =
    hand.tiles.forall(_.isOrphan)

}

case object Shousangen extends Yaku("小三元") {

  def apply(hand: Hand): Int = 2

  def isDefinedAt(hand: Hand): Boolean =
    hand.melds.count(meld => meld.isTriplet && meld.tile.isDragon) >= 2 &&
    hand.melds.exists(meld => meld.isPair && meld.tile.isDragon)

}

case object Honiisou extends Yaku("混一色") {

  def apply(hand: Hand): Int = if (hand.isClosed) 3 else 2

  def isDefinedAt(hand: Hand): Boolean =
    Suit.values.exists(suit => hand.melds.forall(meld => meld.contains(suit) || meld.tile.isHonor)) &&
    !Chiniisou.isDefinedAt(hand)

}

case object Junchantaiyao extends Yaku("純全帯么九") {

  def apply(hand: Hand): Int = if (hand.isClosed) 3 else 2

  def isDefinedAt(hand: Hand): Boolean =
    hand.melds.forall(_.isTerminal)

}

case object Ryanpeikou extends Yaku("二盃口") {

  def apply(hand: Hand): Int = 3

  def isDefinedAt(hand: Hand): Boolean =
    hand.isClosed && hand.grouped.count { case (meld, n) => meld.isSeq && n >= 2 } == 2

}

case object Chiniisou extends Yaku("清一色") {

  def apply(hand: Hand): Int = if (hand.isClosed) 6 else 5

  def isDefinedAt(hand: Hand): Boolean =
    Suit.values.exists(suit => hand.melds.forall(_.contains(suit)))

}

object Yaku {

  val values: Vector[Yaku] =
    Vector(Dora, Yakuhai, Menzenchintsumohou, Pinfu, Iipeikou, Tanyaochuu, Sanshokudoujun, Ikkitsuukan, Chantaiyao, Chiitoitsu, Toitoihou, Sanankou, Sanshokudoukou, Honroutou, Shousangen, Honiisou, Junchantaiyao, Ryanpeikou, Chiniisou)

}
