package mahjongs

import PartialFunction._

sealed trait Yaku extends Any {
  def value: Int
  def decrease: Boolean
  def check(hand: Hand): Boolean
}

object Yaku {
  def values(prevailing: Wind, player: Wind) =
    List(
      List(Selfpick),
      List(HonorTiles(player)),
      List(HonorTiles(prevailing)),
      List(HonorTiles(White)),
      List(HonorTiles(Green)),
      List(HonorTiles(Red)),
      List(AllSimples),
      List(NoPoints),
      List(SevenPairs),
      List(TwoDoubleSeqs, DoubleSeq),
      List(AllTriplets),
      List(Straight),
      List(ThreeTriplets),
      List(ThreeQuads),
      List(ThreeColorSeqs),
      List(ThreeColorTriplets),
      List(LittleDragons),
      List(TerminalsAndHonors),
      List(PureOutside, MixedOutside),
      List(FullFlush, HalfFlush)
    )
  case object Selfpick extends Yaku {
    def value = 1
    def decrease = false
    def check(hand: Hand) = hand.situation.drawn
  }
  case class HonorTiles(tile: Tile) extends AnyVal with Yaku {
    def value = 1
    def decrease = false
    def check(hand: Hand) =
      (hand.concealed ++ hand.melded).exists {
        case Meld.Set(`tile`, n) => n >= 3
        case _ => false
      }
  }
  case object AllSimples extends Yaku {
    def value = 1
    def decrease = false
    def check(hand: Hand) =
      (hand.concealed ++ hand.melded).forall(meld => !meld.isOutside && !meld.isHonor)
  }
  case object MixedOutside extends Yaku {
    def value = 2
    def decrease = true
    def check(hand: Hand) =
      (hand.concealed ++ hand.melded).forall(meld => meld.isOutside || meld.isHonor)
  }
  case object PureOutside extends Yaku {
    def value = 3
    def decrease = true
    def check(hand: Hand) =
      (hand.concealed ++ hand.melded).forall(_.isOutside)
  }
  case object TerminalsAndHonors extends Yaku {
    def value = 2
    def decrease = false
    def check(hand: Hand) =
      (hand.concealed ++ hand.melded).forall(meld => meld.isTerminal || meld.isHonor)
  }
  case object SevenPairs extends Yaku {
    def value = 2
    def decrease = false
    def check(hand: Hand) =
      hand.melded.isEmpty && hand.concealed.forall(_.isInstanceOf[Meld.Pair])
  }
  case object AllTriplets extends Yaku {
    def value = 2
    def decrease = false
    def check(hand: Hand) =
      (hand.concealed ++ hand.melded).count(cond(_) { case Meld.Set(_, n) => n >= 3 }) == 4
  }
  case object NoPoints extends Yaku {
    def value = 1
    def decrease = false
    def check(hand: Hand) =
      hand.waiting == Wait.Sides && hand.melded.isEmpty && hand.concealed.count(_.isInstanceOf[Meld.Seq]) == 4
  }
  case object DoubleSeq extends Yaku {
    def value = 1
    def decrease = false
    def check(hand: Hand) =
      hand.concealed.exists {
        case seq: Meld.Seq => hand.concealed.count(_ == seq) >= 2
        case _ => false
      }
  }
  case object TwoDoubleSeqs extends Yaku {
    def value = 2
    def decrease = false
    def check(hand: Hand) =
      hand.melded.isEmpty && hand.concealed.filter(_.isInstanceOf[Meld.Seq]).groupBy(identity).values.forall(_.size == 2)
  }
  case object Straight extends Yaku {
    def value = 2
    def decrease = true
    def check(hand: Hand) =
      Suit.values.exists(suit => Seq(1, 4, 7).forall(value => (hand.concealed ++ hand.melded).contains(Meld.Seq(suit, value))))
  }
  case object ThreeTriplets extends Yaku {
    def value = 2
    def decrease = false
    def check(hand: Hand) =
      hand.concealed.count(cond(_) { case Meld.Set(_, n) => n >= 3 }) >= 3
  }
  case object ThreeQuads extends Yaku {
    def value = 2
    def decrease = false
    def check(hand: Hand) =
      (hand.concealed ++ hand.melded).count(_.isInstanceOf[Meld.Quad]) >= 3
  }
  case object ThreeColorSeqs extends Yaku {
    def value = 2
    def decrease = true
    def check(hand: Hand) =
      (hand.concealed ++ hand.melded).exists {
        case seq@Meld.Seq(_, start) => Suit.values.forall(suit => (hand.concealed ++ hand.melded).contains(Meld.Seq(suit, start)))
        case _ => false
      }
  }
  case object ThreeColorTriplets extends Yaku {
    def value = 2
    def decrease = false
    def check(hand: Hand) =
      (hand.concealed ++ hand.melded).exists {
        case Meld.Set(Number(_, value), n) if n >= 3 =>
          Suit.values.forall { suit =>
            (hand.concealed ++ hand.melded).exists {
              case Meld.Set(Number(`suit`, `value`), n) => n >= 3
              case _ => false
            }
          }
        case _ => false
      }
  }
  case object LittleDragons extends Yaku {
    def value = 2
    def decrease = false
    def check(hand: Hand) =
      Dragon.values.exists(tile => hand.concealed.contains(Meld.Pair(tile))) && Dragon.values.count { tile =>
        (hand.concealed ++ hand.melded).exists {
          case Meld.Set(`tile`, n) => n >= 3
          case _ => false
        }
      } >= 2
  }
  case object HalfFlush extends Yaku {
    def value = 3
    def decrease = true
    def check(hand: Hand) =
      Suit.values.exists(suit => (hand.concealed ++ hand.melded).forall(meld => meld.contains(suit) || meld.isHonor))
  }
  case object FullFlush extends Yaku {
    def value = 6
    def decrease = true
    def check(hand: Hand) =
      Suit.values.exists(suit => (hand.concealed ++ hand.melded).forall(_.contains(suit)))
  }
}
