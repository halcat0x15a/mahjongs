package mahjongs

import PartialFunction._

case class 手牌(concealed: List[面子], melded: List[面子], waiting: 聴牌)(implicit val situation: 状況) {
  lazy val melds = concealed ++ melded
  lazy val yaku: Map[役, Int] = {
    val yaku = (役.values(situation.prevailing, situation.player) ::: situation.yaku).collect {
      case yaku if yaku.check.isDefinedAt(this) => yaku -> yaku.check(this)
    }.toMap
    yaku -- yaku.keys.collect(役.dependencies)
  }
  lazy val han: Int = yaku.values.sum + situation.dora
  lazy val fu: List[符] =
    if (situation.selfpick && yaku.contains(平和)) {
      List(自摸平和)
    } else if (yaku.contains(七対子)) {
      List(七対子)
    } else if (melded.nonEmpty && 平和.check.isDefinedAt(this)) {
      List(栄平和)
    } else {
      val wait = condOpt(waiting) { case fu: 符 => fu }
      val winning = if (situation.selfpick) Some(自摸符) else if (melded.isEmpty) Some(門前加符) else None
      副底 :: wait.toList ::: concealed.flatMap(門前.parse).toList ::: melded.flatMap(副露.parse).toList ::: winning.toList
    }
  lazy val point: Int =
    ceil(fu.map(_.point).sum, 10)
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
        自摸和(List.fill(3)(ceil(base * 2, 100)))
      else
        自摸和(ceil(base * 2, 100) :: List.fill(2)(ceil(base, 100)))
    else
      if (situation.dealer)
        栄和(ceil(base * 6, 100))
      else
        栄和(ceil(base * 4, 100))
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
    } yield 手牌(concealed.toList, melded.toList, wait)
  }
}
