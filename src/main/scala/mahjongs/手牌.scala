package mahjongs

case class 手牌(concealed: Seq[面子], melded: Seq[面子], waiting: 聴牌)(implicit val situation: 状況) {
  lazy val melds = concealed ++ melded
  lazy val hand: List[役] = 役.values(situation.prevailing, situation.player).filter(_.check(this))
  lazy val han: Int =
    hand.map(hand => if (hand.decrease && melded.nonEmpty) hand.value - 1 else hand.value).sum
  lazy val fu: List[符] = { import 符._
    if (situation.selfpick && hand.contains(役.平和)) List(自摸平和)
    else if (hand.contains(役.七対子)) List(七対子)
    else if (melded.nonEmpty && 役.平和.check(this)) List(栄平和)
    else 副底 :: waiting :: concealed.flatMap(符.parse(true, _)).toList ::: melded.flatMap(符.parse(false, _)).toList ::: (if (hand.contains(役.門前清自摸和)) List(自摸符) else if (melded.isEmpty) List(門前加符) else Nil)
  }
  lazy val base =
    if (han >= 13) 8000
    else if (han >= 11) 6000
    else if (han >= 8) 4000
    else if (han >= 6) 3000
    else if (han >= 5) 2000
    else math.min(ceil(fu.map(_.value).sum, 10) * Math.pow(2, han + 2), 2000)
  lazy val score: 和了 =
    if (situation.selfpick)
      if (situation.dealer)
        親自摸和(ceil(base * 2, 100).toInt)
      else
        子自摸和(ceil(base * 2, 100).toInt, ceil(base, 100).toInt)
    else
      if (situation.dealer)
        栄和(ceil(base * 6, 100).toInt)
      else
        栄和(ceil(base * 4, 100).toInt)
  private def ceil(value: Double, n: Int): Double = math.ceil(value / n) * n
}

object 手牌 {
  def combinations(tiles: Seq[牌]): List[List[面子]] =
    tiles match {
      case tile +: tiles =>
        val seq = tile match {
          case number: 数牌 if number.sequence.tail.forall(tiles.contains) =>
            combinations(tiles diff number.sequence.tail).map(順子(number) :: _)
          case _ => Nil
        }
        val sets = for {
          i <- 0 until tiles.count(_ == tile)
          melds <- combinations(tiles diff List.fill(i + 1)(tile))
          meld = i match {
            case 0 => 対子(tile)
            case 1 => 刻子(tile)
            case 2 => 槓子(tile)
          }
        } yield meld :: melds
        seq ++ sets
      case _ => List(Nil)
    }
  def patterns(tile: 牌, concealed: Seq[牌], melded: Seq[面子])(implicit situation: 状況): Seq[手牌] = {
    val tiles = (tile -> true) +: concealed.map(_ -> false)
    for {
      melds <- combinations(tiles.map(_._1))
      if melds.size + melded.size == 5
      meld <- melds.find(_.tile == tile).toList
      wait <- 聴牌.parse(tile, meld).toList
    } yield 手牌(melds, melded, wait)
  }
}
