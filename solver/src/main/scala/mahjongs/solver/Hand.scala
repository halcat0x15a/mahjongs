package mahjongs.solver

case class Hand(waiting: Wait, closedMelds: List[Meld], openMelds: List[Meld], situation: Situation) {

  val melds: List[Meld] = closedMelds ::: openMelds

  val tiles: List[Tile] = melds.flatMap(_.tiles)

  val grouped: Map[Meld, Int] = melds.groupBy(identity).mapValues(_.size)

  val isClosed: Boolean = openMelds.isEmpty

  val isNoPoints: Boolean =
    waiting == Ryanmen && melds.count(_.isSeq) == 4 && melds.exists(meld => meld.isPair && !situation.isHonor(meld.tile))

  val isSevenPairs: Boolean =
    closedMelds.count(_.isPair) == 7

  val fu: Int =
    if (isNoPoints && !isClosed)
      30
    else if (isSevenPairs)
      25
    else
      ceil(20 + waiting.fu + melds.map(Meld.fu(_, situation)).sum + situation.fu(isClosed), 10)

  val yaku: Vector[Yaku] =
    Yaku.values.filter(_.isDefinedAt(this))

  val han: Int = yaku.map(_(this)).sum

  val points: Int = fu * math.pow(2, han + 2).toInt

  val basicPoints: Int =
    if (han >= 13)
      8000
    else if (han >= 11)
      6000
    else if (han >= 8)
      4000
    else if (han >= 6)
      3000
    else
      math.min(points, 2000)

  val win: Win = Win.calc(basicPoints, situation)

}

object Hand {

  def histgram(tiles: Seq[Tile]): Vector[Int] =
    tiles.foldLeft(Vector.fill(34)(0))((histgram, tile) => histgram.updated(tile.index, histgram(tile.index) + 1))

  def calc(tile: Tile, tiles: Seq[Tile], melds: List[Meld], situation: Situation): Option[Hand] = {
    val hands = parse(histgram(tile +: tiles)).collect {
      case closed if closed.size + melds.size == 5 =>
        closed.flatMap { meld => Wait.calc(tile, meld) }.map(Hand(_, closed, melds, situation))
    }.flatten
    if (hands.isEmpty) None else Some(hands.maxBy(_.points))
  }

  def set(histgram: Vector[Int], size: Int): Iterator[(Meld, Vector[Int])] =
    histgram.toIterator.zipWithIndex.collect {
      case (n, i) if n >= size => Meld.set(Tile.values(i), size).map((_, histgram.updated(i, n - size)))
    }.flatten

  def seq(histgram: Vector[Int]): Iterator[(Meld, Vector[Int])] =
    histgram.toIterator.take(27).zipWithIndex.sliding(9, 9).flatMap(_.sliding(3).collect {
      case (a, i) +: (b, j) +: (c, k) +: _ if a > 0 && b > 0 && c > 0 =>
        (Shuntsu(Num.values(i), true), histgram.updated(i, a - 1).updated(j, b - 1).updated(k, c - 1))
    })

  def sevenPairs(histgram: Vector[Int]): Option[List[Meld]] =
    if (histgram.count(_ == 2) == 7)
      Some(histgram.zipWithIndex.collect { case (2, i) => Toitsu(Tile.values(i)) }(collection.breakOut))
    else
      None

  def parse(histgram: Vector[Int]): Iterator[List[Meld]] = {
    def go(histgram: Vector[Int]): Iterator[List[Meld]] =
      if (histgram.forall(_ == 0))
        Iterator(Nil)
      else
        (set(histgram, 3).take(1) ++ seq(histgram).take(1)).flatMap { case (meld, histgram) => go(histgram).map(meld :: _) }
    set(histgram, 2).flatMap { case (meld, histgram) => go(histgram).map(meld :: _) }
  }

}
