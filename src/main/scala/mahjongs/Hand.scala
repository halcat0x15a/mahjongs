package mahjongs

case class Hand(concealed: Seq[Meld], melded: Seq[Meld], waiting: Wait)(implicit val situation: Situation) {
  lazy val yaku: Seq[Yaku] = Yaku.values(situation.prevailing, situation.player).map(yaku => yaku.find(_.check(this))).flatten
  lazy val han: Int =
    yaku.map(yaku => if (yaku.decrease && melded.nonEmpty) yaku.value - 1 else yaku.value).sum
  lazy val winning: Option[Fu] =
    situation.winning match {
      case Drawn => Some(Fu.Drawn)
      case Discard if melded.isEmpty => Some(Fu.ConcealedDiscard)
      case _ => None
    }
  lazy val fu: Seq[Fu] =
    if (winning == Drawn && yaku.contains(Yaku.NoPoints)) List(Fu.DrawnNoPoints)
    else if (yaku.contains(Yaku.SevenPairs)) List(Fu.SevenPairs)
    else if (melded.nonEmpty && Yaku.NoPoints.check(this)) List(Fu.MeldedNoPoints)
    else Fu.Base +: Fu.Wait(waiting) +: (concealed.map(Fu.Meld(true, _)) ++ melded.map(Fu.Meld(false, _)) ++ winning.toList)
  lazy val base =
    if (han >= 13) 8000
    else if (han >= 11) 6000
    else if (han >= 8) 4000
    else if (han >= 6) 3000
    else if (han >= 5) 2000
    else math.min(ceil(fu.map(_.value).sum, 10) * Math.pow(2, han + 2), 2000)
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
  def patterns(tile: Tile, concealed: Seq[Tile], quads: Seq[Meld.Quad], melded: Seq[Meld])(implicit situation: Situation): Seq[Hand] = {
    val tiles = (tile -> true) +: concealed.map(_ -> false)
    for {
      melds <- Hand.combinations(tiles.map(_._1))
      if melds.size + quads.size + melded.size == 5
      wait <- Wait.find(tiles.takeWhile(_._2).size, melds).toSeq
    } yield Hand(melds ++ quads, melded, wait)
  }
}
