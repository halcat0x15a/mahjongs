package mahjongs

import PartialFunction._

sealed trait Combination extends Any {
  def value: Int
  def decrease: Boolean
  def check(concealed: Seq[Meld], melded: Seq[Meld]): Boolean
}

object Combination {
  def values(prevailing: Wind, player: Wind) =
    List(
      List(Selfpick),
      List(HonorTiles(player)),
      List(HonorTiles(prevailing)),
      List(HonorTiles(White)),
      List(HonorTiles(Green)),
      List(HonorTiles(Red)),
      List(AllSimples),
      List(AllSeqs),
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
  def check(selfpick: Boolean, wait: Wait)(combination: Combination) =
    combination match{
      case Selfpick => selfpick
      case AllSimples => wait == Wait.Sides
      case _ => true
    }
  case object Selfpick extends Combination {
    def value = 1
    def decrease = false
    def check(concealed: Seq[Meld], melded: Seq[Meld]) = true
  }
  case class HonorTiles(tile: Tile) extends AnyVal with Combination {
    def value = 1
    def decrease = false
    def check(concealed: Seq[Meld], melded: Seq[Meld]) =
      (concealed ++ melded).exists {
        case Meld.Set(`tile`, n) => n >= 3
        case _ => false
      }
  }
  case object AllSimples extends Combination {
    def value = 1
    def decrease = false
    def check(concealed: Seq[Meld], melded: Seq[Meld]) =
      (concealed ++ melded).forall(meld => !meld.isOutside && !meld.isHonor)
  }
  case object MixedOutside extends Combination {
    def value = 2
    def decrease = true
    def check(concealed: Seq[Meld], melded: Seq[Meld]) =
      (concealed ++ melded).forall(meld => meld.isOutside || meld.isHonor)
  }
  case object PureOutside extends Combination {
    def value = 3
    def decrease = true
    def check(concealed: Seq[Meld], melded: Seq[Meld]) =
      (concealed ++ melded).forall(_.isOutside)
  }
  case object TerminalsAndHonors extends Combination {
    def value = 2
    def decrease = false
    def check(concealed: Seq[Meld], melded: Seq[Meld]) =
      (concealed ++ melded).forall(meld => meld.isTerminal || meld.isHonor)
  }
  case object SevenPairs extends Combination {
    def value = 2
    def decrease = false
    def check(concealed: Seq[Meld], melded: Seq[Meld]) =
      melded.isEmpty && concealed.forall(_.isInstanceOf[Meld.Pair])
  }
  case object AllTriplets extends Combination {
    def value = 2
    def decrease = false
    def check(concealed: Seq[Meld], melded: Seq[Meld]) =
      (concealed ++ melded).count(cond(_) { case Meld.Set(_, n) => n >= 3 }) == 4
  }
  case object AllSeqs extends Combination {
    def value = 1
    def decrease = false
    def check(concealed: Seq[Meld], melded: Seq[Meld]) =
      melded.isEmpty && concealed.count(_.isInstanceOf[Meld.Seq]) == 4
  }
  case object DoubleSeq extends Combination {
    def value = 1
    def decrease = false
    def check(concealed: Seq[Meld], melded: Seq[Meld]) =
      concealed.exists {
        case seq: Meld.Seq => concealed.count(_ == seq) >= 2
        case _ => false
      }
  }
  case object TwoDoubleSeqs extends Combination {
    def value = 2
    def decrease = false
    def check(concealed: Seq[Meld], melded: Seq[Meld]) =
      melded.isEmpty && concealed.filter(_.isInstanceOf[Meld.Seq]).groupBy(identity).values.forall(_.size == 2)
  }
  case object Straight extends Combination {
    def value = 2
    def decrease = true
    def check(concealed: Seq[Meld], melded: Seq[Meld]) =
      Suit.values.exists(suit => Seq(1, 4, 7).forall(value => (concealed ++ melded).contains(Meld.Seq(suit, value))))
  }
  case object ThreeTriplets extends Combination {
    def value = 2
    def decrease = false
    def check(concealed: Seq[Meld], melded: Seq[Meld]) =
      melded.isEmpty && concealed.count(cond(_) { case Meld.Set(_, n) => n >= 3 }) >= 3
  }
  case object ThreeQuads extends Combination {
    def value = 2
    def decrease = false
    def check(concealed: Seq[Meld], melded: Seq[Meld]) =
      (concealed ++ melded).count(_.isInstanceOf[Meld.Quad]) >= 3
  }
  case object ThreeColorSeqs extends Combination {
    def value = 2
    def decrease = true
    def check(concealed: Seq[Meld], melded: Seq[Meld]) =
      (concealed ++ melded).exists {
        case seq@Meld.Seq(_, start) => Suit.values.forall(suit => (concealed ++ melded).contains(Meld.Seq(suit, start)))
        case _ => false
      }
  }
  case object ThreeColorTriplets extends Combination {
    def value = 2
    def decrease = false
    def check(concealed: Seq[Meld], melded: Seq[Meld]) =
      (concealed ++ melded).exists {
        case Meld.Set(Number(_, value), n) if n >= 3 =>
          Suit.values.forall { suit =>
            (concealed ++ melded).exists {
              case Meld.Set(Number(`suit`, `value`), n) => n >= 3
              case _ => false
            }
          }
        case _ => false
      }
  }
  case object LittleDragons extends Combination {
    def value = 2
    def decrease = false
    def check(concealed: Seq[Meld], melded: Seq[Meld]) =
      Dragon.values.exists(tile => concealed.contains(Meld.Pair(tile))) && Dragon.values.count { tile =>
        (concealed ++ melded).exists {
          case Meld.Set(`tile`, n) => n >= 3
          case _ => false
        }
      } >= 2
  }
  case object HalfFlush extends Combination {
    def value = 3
    def decrease = true
    def check(concealed: Seq[Meld], melded: Seq[Meld]) =
      Suit.values.exists(suit => (concealed ++ melded).forall(meld => meld.contains(suit) || meld.isHonor))
  }
  case object FullFlush extends Combination {
    def value = 6
    def decrease = true
    def check(concealed: Seq[Meld], melded: Seq[Meld]) =
      Suit.values.exists(suit => (concealed ++ melded).forall(_.contains(suit)))
  }
}
