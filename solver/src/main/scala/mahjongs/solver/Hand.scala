package mahjongs.solver

import scala.collection.breakOut

case class Hand(waiting: Wait, closedMelds: List[Meld], openMelds: List[Meld], isDealer: Boolean, isSelfDrawn: Boolean, seatWind: Tile, prevailingWind: Tile, dora: Int) {

  val melds: List[Meld] = closedMelds ::: openMelds

  val tiles: List[Tile] = melds.flatMap(_.tiles)

  val grouped: Map[Meld, Int] = melds.groupBy(identity).mapValues(_.size)

  val isClosed: Boolean = openMelds.isEmpty

  val isNoPoints: Boolean =
    waiting == Ryanmen && melds.count(_.isSeq) == 4 && melds.exists(meld => meld.isPair && (Tile.dragon :+ seatWind :+ prevailingWind).contains(meld.tile))

  val isSevenPairs: Boolean =
    closedMelds.count(_.isPair) == 7

  val fu: Int =
    if (isNoPoints && !isClosed)
      30
    else if (isSevenPairs)
      25
    else
      20 + melds.map(Meld.fu(_, seatWind, prevailingWind)).sum + waiting.fu

  val yaku: Vector[Yaku] =
    Yaku.values.filter(_.calculate.isDefinedAt(this))

  val han: Int = yaku.map(_.calculate(this)).sum

  val score: Win = Win.calc(fu, han, isDealer, isSelfDrawn)

  private def ceil(value: Double): Int = (math.ceil(value / 100) * 100).toInt

}

object Hand {

  def calc(tile: Tile, tiles: Seq[Tile], melds: List[Meld], isDealer: Boolean, isSelfDrawn: Boolean, seatWind: Tile, prevailingWind: Tile, dora: Int): Hand = {
    val histgram = (tile +: tiles).foldLeft(Vector.fill(34)(0))((histgram, tile) => histgram.updated(tile.index, histgram(tile.index) + 1))
    parse(histgram).collect {
      case closed if closed.size + melds.size == 5 =>
        closed.flatMap { meld => Wait.calc(tile, meld) }.map(Hand(_, closed, melds, isDealer, isSelfDrawn, seatWind, prevailingWind, dora))
    }.flatten.maxBy(_.score.points)
  }

  def set(histgram: Vector[Int], size: Int): Iterator[(Meld, Vector[Int])] =
    histgram.toIterator.zipWithIndex.collect {
      case (n, i) if n >= size => Meld.set(Tile.values(i), size).map((_, histgram.updated(i, n - size)))
    }.flatten

  def seq(histgram: Vector[Int]): Iterator[(Meld, Vector[Int])] =
    histgram.toIterator.take(27).zipWithIndex.sliding(9, 9).flatMap(_.sliding(3).collect {
      case (a, i) +: (b, j) +: (c, k) +: _ if a > 0 && b > 0 && c > 0 =>
        (Shuntsu(Tile.num(i), true), histgram.updated(i, a - 1).updated(j, b - 1).updated(k, c - 1))
    })

  def sevenPairs(histgram: Vector[Int]): Option[List[Meld]] =
    if (histgram.count(_ == 2) == 7)
      Some(histgram.zipWithIndex.collect { case (2, i) => Toitsu(Tile.values(i)) }(breakOut))
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
