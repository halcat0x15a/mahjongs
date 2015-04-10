package mahjongs

case class 手牌(concealed: Seq[面子], melded: Seq[面子], waiting: 聴牌)(implicit val situation: 状況) {
  lazy val melds = concealed ++ melded
  lazy val yaku: List[役] = {
    val yaku = (役.values(situation.prevailing, situation.player) ::: situation.yaku.filter(_.check(this))).filter(_.check(this))
    yaku.diff(yaku.collect(役.dependencies))
  }
  lazy val han: Int =
    yaku.map(yaku => if (yaku.decrease && melded.nonEmpty) yaku.value - 1 else yaku.value).sum + situation.dora
  lazy val fu: List[符] = { import 符._
    if (situation.selfpick && yaku.contains(役.平和)) List(自摸平和)
    else if (yaku.contains(役.七対子)) List(七対子)
    else if (melded.nonEmpty && 役.平和.check(this)) List(栄平和)
    else 副底 :: (if (waiting.value > 0) List(waiting) else Nil) ::: concealed.flatMap(符.parse(true, _)).toList ::: melded.flatMap(符.parse(false, _)).toList ::: (if (situation.selfpick) List(自摸符) else if (melded.isEmpty) List(門前加符) else Nil)
  }
  lazy val point: Int =
    ceil(fu.map(_.value).sum, 10)
  lazy val base: Int =
    if (han >= 13) 8000
    else if (han >= 11) 6000
    else if (han >= 8) 4000
    else if (han >= 6) 3000
    else if (han >= 5) 2000
    else math.min(point * Math.pow(2, han + 2), 2000).toInt
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
  override def toString = s"手牌($concealed, $melded, $waiting)($situation)"
  private def ceil(value: Double, n: Int): Int = (math.ceil(value / n) * n).toInt
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
  def patterns(winning: Int, concealed: Seq[牌], melded: Seq[面子])(implicit situation: 状況): Seq[手牌] = {
    val tile = concealed(winning)
    for {
      concealed <- combinations(concealed)
      if (concealed ++ melded).flatMap(_.tiles).size >= 14 &&
         concealed.size + melded.size == 5 ||
         concealed.forall(_.isInstanceOf[対子]) && melded.isEmpty && concealed.size == 7
      meld <- concealed.find(_.tiles.contains(tile)).toList
      wait <- 聴牌.parse(tile, meld).toList
    } yield 手牌(concealed, melded, wait)
  }
}
