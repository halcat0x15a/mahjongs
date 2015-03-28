package mahjongs

sealed trait Fu {
  val value: Int
}

object Fu {
  case object Base extends Fu { val value = 20 }
  case object ConcealedDiscard extends Fu { val value = 10 }
  case object Drawn extends Fu { val value = 2 }
  case object DrawnNoPoints extends Fu { val value = 20 }
  case class Wait(waiting: mahjongs.Wait) extends Fu {
    import mahjongs.Wait._
    val value: Int =
      waiting match {
        case Sides | Double => 0
        case Single | Closed | Edge => 2
      }
  }
  case class Meld(concealed: Boolean, meld: mahjongs.Meld)(implicit situation: Situation) extends Fu {
    import mahjongs.Meld._
    val value: Int =
      meld match {
        case Pair(dragon: Dragon) => 2
        case Pair(wind: Wind) if wind == situation.player || wind == situation.prevailing => 2
        case Triplet(tile) =>
          if (concealed && (tile.isTerminal || tile.isHonor)) 8
          else if (!concealed && !tile.isTerminal && !tile.isHonor) 2
          else 4
        case Quad(tile) =>
          if (concealed && (tile.isTerminal || tile.isHonor)) 32
          else if (!concealed && !tile.isTerminal && !tile.isHonor) 8
          else 16
        case _ => 0
      }
  }
  case object SevenPairs extends Fu { val value = 25 }
  case object MeldedNoPoints extends Fu { val value = 30 }
}
