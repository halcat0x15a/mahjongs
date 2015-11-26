package mahjongs.solver

case class Yaku(name: String, calculate: PartialFunction[Hand, Int])

object Yaku {

  def isTerminal(meld: Meld): Boolean =
    meld match {
      case Shuntsu(Num(_, 1 | 7), _) => true
      case _ => false
    }

  val values: Vector[Yaku] =
    Vector(
      Yaku("平和", {
        case hand if hand.isClosed && hand.isNoPoints => 1
      }),
      Yaku("断么九", {
        case hand if hand.isClosed && hand.tiles.forall(!_.isOrphan) => 1
      }),
      Yaku("一盃口", {
        case hand if hand.isClosed && hand.grouped.exists { case (meld, n) => meld.isSeq && n >= 2 } => 1
      }),
      Yaku("三色同順", {
        case hand if (1 to 7).exists(n => Tile.suit.forall(suit => hand.melds.exists(_.tile == Num(suit, n)))) =>
          if (hand.isClosed) 2 else 1
      }),
      Yaku("一気通貫", {
        case hand if Tile.suit.exists(suit => (1 to 9 by 3).forall(n => hand.melds.exists(meld => meld.isSeq && meld.tile == Num(suit, n)))) =>
          if (hand.isClosed) 2 else 1
      }),
      Yaku("混全帯么九", {
        case hand if hand.melds.forall(meld => isTerminal(meld) || meld.tile.isOrphan) =>
          if (hand.isClosed) 2 else 1
      }),
      Yaku("七対子", {
        case hand if hand.isSevenPairs => 2
      }),
      Yaku("対々和", {
        case hand if hand.melds.count(_.isTriplet) == 4 => 2
      }),
      Yaku("三暗刻", {
        case hand if hand.closedMelds.count(_.isTriplet) >= 3 => 2
      }),
      Yaku("三色同刻", {
        case hand if (1 to 9).exists(n => Tile.suit.forall(suit => hand.melds.exists(_.tile == Num(suit, n)))) => 2
      }),
      Yaku("混老頭", {
        case hand if hand.tiles.forall(_.isOrphan) => 2
      }),
      Yaku("小三元", {
        case hand if hand.melds.count(meld => meld.isTriplet && meld.tile.isDragon) >= 2 && hand.melds.exists(meld => meld.isPair && meld.tile.isDragon) => 2
      }),
      Yaku("混一色", {
        case hand if Tile.suit.exists(suit => hand.melds.forall(meld => meld.contains(suit) || meld.tile.isHonor)) =>
          if (hand.isClosed) 3 else 2
      }),
      Yaku("純全帯么九", {
        case hand if hand.melds.forall(isTerminal) =>
          if (hand.isClosed) 3 else 2
      }),
      Yaku("二盃口", {
        case hand if hand.isClosed && hand.grouped.count { case (meld, n) => meld.isSeq && n >= 2 } == 2 => 3
      }),
      Yaku("清一色", {
        case hand if Tile.suit.exists(suit => hand.melds.forall(_.contains(suit))) =>
          if (hand.isClosed) 6 else 5
      })
    )

}
