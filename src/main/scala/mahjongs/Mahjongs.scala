package mahjongs

case class Hand(tile: Tile, concealed: Seq[Tile], quads: Seq[Meld.Quad], melded: Seq[Meld])

object Mahjongs {
  case class Result(winning: Winning, han: Int, fu: Int, combinations: Seq[Combination], melds: Seq[Meld])
  type Elem = Tile
  def ceil(value: Double, n: Int): Double = math.ceil(value / n) * n
  def apply(hand: Hand, selfpick: Boolean, dealer: Boolean, prevailing: Wind, player: Wind, addition: Int) = {
    val tiles = ((hand.tile -> true) +: hand.concealed.map(_ -> false)).sorted
    val scores = for {
      melds <- combinations(tiles.map(_._1))
      if melds.size + hand.quads.size + hand.melded.size == 5
      wait <- Wait.find(tiles.takeWhile(_._2).size, melds).toSeq
    } yield {
      val combinations = Combination.values(prevailing, player).map(combinations => combinations.find(_.check(melds ++ hand.quads, hand.melded))).flatten.filter(Combination.check(selfpick, wait))
      val han = combinations.map(combination => if (combination.decrease && hand.melded.nonEmpty) combination.value - 1 else combination.value).sum + addition
      val fu =
        if (selfpick && combinations.contains(Combination.AllSeqs)) 20
        else if (combinations.contains(Combination.SevenPairs)) 25
        else if (Combination.AllSeqs.check(melds ++ hand.quads, hand.melded) && wait == Wait.Sides) 30
        else ceil((20 + (if (hand.melded.isEmpty) (if (selfpick) 2 else 10) else 0) + wait.fu + (melds ++ hand.quads).map(_.fu).sum + hand.melded.map(_.fu / 2).sum).toDouble, 10).toInt
      val base =
        if (han >= 13) 8000
        else if (han >= 11) 6000
        else if (han >= 8) 4000
        else if (han >= 6) 3000
        else if (han >= 5) 2000
        else math.min(fu * Math.pow(2, han + 2), 2000)
      val winning =
        if (dealer) {
          if (selfpick)
            DealerDrawn(ceil(base * 2, 100).toInt)
          else
            Discard(ceil(base * 6, 100).toInt)
        } else {
          if (selfpick)
            NonDealerDrawn(ceil(base, 100).toInt, ceil(base * 2, 100).toInt)
          else
            Discard(ceil(base * 4, 100).toInt)
        }
      Result(winning, han, fu, combinations, melds ++ hand.quads ++ hand.melded)
    }
    if (scores.nonEmpty)
      Some(scores.maxBy(_.winning.value))
    else
      None
  }
  def combinations(tiles: Seq[Tile]): List[List[Meld]] =
    tiles match {
      case tile +: tiles =>
        lazy val seq: List[List[Meld]] = tile match {
          case Number(suit, value) if tiles.contains(suit(value + 1)) && tiles.contains(suit(value + 2)) =>
            combinations(tiles diff List(suit(value + 1), suit(value + 2))).map(Meld.Seq(suit, value) :: _)
          case _ => Nil
        }
        lazy val sets = (0 until tiles.count(_ == tile)).flatMap(i => combinations(tiles diff List.fill(i + 1)(tile)).map(Meld.Set(tile, i + 2) :: _))
        seq ++ sets
      case _ => List(Nil)
    }
}
