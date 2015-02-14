package mahjongs

sealed trait Hand {
  def value: Value
  def apply(melds: Seq[Meld], wait: Wait): Boolean
}

sealed trait Value

case class Closed(value: Int) extends Value

case class Open(value: Int, decrease: Boolean) extends Value

case object AllSimples extends Hand {
  def value = Closed(1)
  def apply(melds: Seq[Meld], wait: Wait) =
    melds.forall(_.tiles.forall(tile => !tile.isTerminal && !tile.isHonor))
}

case object AllRuns extends Hand {
  def value = Closed(1)
  def apply(melds: Seq[Meld], wait: Wait) =
    melds.count(_.isInstanceOf[Run]) == 4 && wait == Wait.Sides
}

case object DoubleRun extends Hand {
  def value = Closed(1)
  def apply(melds: Seq[Meld], wait: Wait) =
    melds.exists(meld => meld.isInstanceOf[Run] && melds.count(_ == meld) >= 2)
}

case object TwoDoubleRuns extends Hand {
  def value = Closed(2)
  def apply(melds: Seq[Meld], wait: Wait) =
    melds.count(meld => meld.isInstanceOf[Run] && melds.count(_ == meld) >= 2) >= 4
}

case object SevenPairs extends Hand {
  def value = Closed(2)
  def apply(melds: Seq[Meld], wait: Wait) =
    melds.forall { case Set(tiles) => tiles.size == 2; case _ => false }
}

case object AllTriplets extends Hand {
  def value = Open(2, false)
  def apply(melds: Seq[Meld], wait: Wait) =
    melds.count { case Set(tiles) => tiles.size > 2; case _ => false } == 4
}

case object Straight extends Hand {
  def value = Open(2, true)
  def apply(melds: Seq[Meld], wait: Wait) =
    Suit.values.exists(suit => Seq(1, 4, 7).forall(value => melds.exists { case Run(Number(`suit`, `value`) +: _) => true; case _ => false }))
}

case object ThreeTriplets extends Hand {
  def value = Closed(2)
  def apply(melds: Seq[Meld], wait: Wait) =
    melds.count { case Set(tiles) => tiles.size == 3; case _ => false } == 3
}

case object ThreeQuads extends Hand {
  def value = Open(2, false)
  def apply(melds: Seq[Meld], wait: Wait) =
    melds.count { case Set(tiles) => tiles.size == 4; case _ => false } == 3
}

case object ThreeColor extends Hand {
  def value = Open(2, true)
  def apply(melds: Seq[Meld], wait: Wait) =
    melds.exists {
      case Run(Number(_, value) +: _) =>
        Suit.values.map(suit => Run(value to value + 2 map (value => Number(suit, value)))).forall(melds.contains)
      case Set(tiles@(Number(_, value) :: _)) if tiles.size > 2 =>
        Suit.values.map(suit => Set(List.fill(tiles.size)(Number(suit, value)))).forall(melds.contains)
      case _ =>
        false
    }
}

case object LittleDragons extends Hand {
  def value = Open(2, false)
  def apply(melds: Seq[Meld], wait: Wait) =
    melds.flatMap(_.tiles).count(Dragon.values.contains) >= 8
}

case object TerminalsAndHonors extends Hand {
  def value = Open(2, false)
  def apply(melds: Seq[Meld], wait: Wait) =
    melds.flatMap(_.tiles).forall(tile => tile.isTerminal || tile.isHonor)
}

case object MixedOutside extends Hand {
  def value = Open(2, true)
  def apply(melds: Seq[Meld], wait: Wait) =
    melds.forall {
      case Set(tile +: _) => tile.isTerminal || tile.isHonor
      case Run(Number(_, 1) +: _) => true
      case Run(Number(_, 7) +: _) => true
      case _ => false
    }
}

case object PureOutside extends Hand {
  def value = Open(3, true)
  def apply(melds: Seq[Meld], wait: Wait) =
    melds.forall {
      case Set(tile +: _) => tile.isTerminal
      case Run(Number(_, 1) +: _) => true
      case Run(Number(_, 7) +: _) => true
      case _ => false
    }
}

case object HalfFlush extends Hand {
  def value = Open(3, true)
  def apply(melds: Seq[Meld], wait: Wait) =
    melds.flatMap(_.tiles) match {
      case tiles@(Number(suit, _) +: _) =>
        tiles forall {
          case Number(`suit`, _) => true
          case tile => tile.isHonor
        }
    }
}

case object FullFlush extends Hand {
  def value = Open(6, true)
  def apply(melds: Seq[Meld], wait: Wait) =
    melds.flatMap(_.tiles) match {
      case tiles@(Number(suit, _) +: _) =>
        tiles forall {
          case Number(`suit`, _) => true
          case _ => false
        }
    }
}
