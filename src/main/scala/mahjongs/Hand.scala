package mahjongs

case class Hand(concealed: Seq[Meld], melded: Seq[Meld], waiting: Wait)(implicit val situation: Situation) {
  lazy val yaku: Seq[Yaku] = Yaku.values(situation.prevailing, situation.player).map(yaku => yaku.find(_.check(this))).flatten
  lazy val han: Int =
    yaku.map(yaku => if (yaku.decrease && melded.nonEmpty) yaku.value - 1 else yaku.value).sum
  lazy val fu: Seq[Fu] =
    if (situation.drawn && yaku.contains(Yaku.NoPoints)) List(Fu.DrawnNoPoints)
    else if (yaku.contains(Yaku.SevenPairs)) List(Fu.SevenPairs)
    else if (melded.nonEmpty && Yaku.NoPoints.check(this)) List(Fu.MeldedNoPoints)
    else Fu.Base +: Fu.Wait(waiting) +: (concealed.map(Fu.Meld(true, _)) ++ melded.map(Fu.Meld(false, _)) ++ (if (situation.drawn) List(Fu.Drawn) else if (melded.isEmpty) List(Fu.ConcealedDiscard) else Nil))
  lazy val base =
    if (han >= 13) 8000
    else if (han >= 11) 6000
    else if (han >= 8) 4000
    else if (han >= 6) 3000
    else if (han >= 5) 2000
    else math.min(ceil(fu.map(_.value).sum, 10) * Math.pow(2, han + 2), 2000)
  lazy val score: Score =
    if (situation.drawn)
      if (situation.dealer)
        DealerDrawn(ceil(base * 2, 100).toInt)
      else
        NonDealerDrawn(ceil(base * 2, 100).toInt, ceil(base, 100).toInt)
    else
      if (situation.dealer)
        Discard(ceil(base * 6, 100).toInt)
      else
        Discard(ceil(base * 4, 100).toInt)
}

object Hand {
  def combinations(tiles: Seq[Tile]): List[List[Meld]] =
    tiles match {
      case tile +: tiles =>
        val seq = tile match {
          case Number(suit, value) if tiles.contains(suit(value + 1)) && tiles.contains(suit(value + 2)) =>
            combinations(tiles diff List(suit(value + 1), suit(value + 2))).map(Meld.Seq(suit, value) :: _)
          case _ => Nil
        }
        val sets = for {
          i <- 0 until tiles.count(_ == tile)
          melds <- combinations(tiles diff List.fill(i + 1)(tile))
        } yield Meld.Set(tile, i + 2) :: melds
        seq ++ sets
      case _ => List(Nil)
    }
  def patterns(tile: Tile, concealed: Seq[Tile], melded: Seq[Meld])(implicit situation: Situation): Seq[Hand] = {
    val tiles = (tile -> true) +: concealed.map(_ -> false)
    for {
      melds <- Hand.combinations(tiles.map(_._1))
      if melds.size + melded.size == 5
      wait <- Wait.find(tiles.takeWhile(_._2).size, melds).toSeq
    } yield Hand(melds, melded, wait)
  }
}
