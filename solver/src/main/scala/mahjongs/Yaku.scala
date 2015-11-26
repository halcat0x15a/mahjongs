package mahjongs

sealed trait Yaku {

  def han: Int

  def isDefined: Boolean

}

trait YakuApi { self: FuApi =>

  val waiting: Wait

  val openMelds: List[Meld]

  val closedMelds: List[Meld]

  lazy val melds: List[Meld] = closedMelds ::: openMelds

  lazy val tiles: List[Tile] = melds.flatMap(_.tiles)

  lazy val isClosed: Boolean = openMelds.isEmpty

  lazy val isOpen: Boolean = !isClosed

  lazy val allYaku: List[Yaku] = List(ドラ, 場風, 自風, 白, 發, 中, 門前清自摸和, 海底摸月, 河底撈魚, 平和, 断么九, 一盃口, 七対子, 混全帯么九, 対々和, 一気通貫, 三暗刻, 三槓子, 三色同刻, 三色同順, 混老頭, 小三元, 二盃口, 純全帯么九, 混一色, 清一色, 国士無双, 四暗刻, 大三元, 字一色, 小四喜, 大四喜, 緑一色, 清老頭, 四槓子, 九蓮宝燈)

  def 喰い下がり(n: Int): Int = if (isClosed) n else n - 1

  import mahjong._

  lazy val isNoPointsHand: Boolean = melds.forall(meld => fu(meld).value == 0)

  def find(tile: Tile): Option[Meld] = melds.find(_.tile == tile)

  case object ドラ extends Yaku {
    lazy val han: Int = dora
    lazy val isDefined: Boolean = dora > 0
  }

  case object 場風 extends Yaku {
    val han: Int = 1
    lazy val isDefined: Boolean = find(prevailingWind).exists(_.isTriplet)
  }

  case object 自風 extends Yaku {
    val han: Int = 1
    lazy val isDefined: Boolean = find(seatWind).exists(_.isTriplet)
  }

  case object 白 extends Yaku {
    val han: Int = 1
    lazy val isDefined: Boolean = find(White).exists(_.isTriplet)
  }

  case object 發 extends Yaku {
    val han: Int = 1
    lazy val isDefined: Boolean = find(Green).exists(_.isTriplet)
  }

  case object 中 extends Yaku {
    val han: Int = 1
    lazy val isDefined: Boolean = find(Red).exists(_.isTriplet)
  }

  case object 門前清自摸和 extends Yaku {
    val han: Int = 1
    lazy val isDefined: Boolean = isSelfDrawn && isClosed
  }

  case object 海底摸月 extends Yaku {
    val han: Int = 1
    lazy val isDefined: Boolean = isLast && isSelfDrawn
  }

  case object 河底撈魚 extends Yaku {
    val han: Int = 1
    lazy val isDefined: Boolean = isLast && isDiscard
  }

  case object 平和 extends Yaku {
    val han: Int = 1
    lazy val isDefined: Boolean = waiting == Wait.両面 && isClosed && isNoPointsHand
  }

  case object 断么九 extends Yaku {
    val han: Int = 1
    lazy val isDefined: Boolean = isClosed && tiles.forall(tile => tile.isNumber && !tile.isTerminal)
  }

  case object 一盃口 extends Yaku {
    val han: Int = 1
    lazy val isDefined: Boolean = !二盃口.isDefined && closedMelds.filter(_.isSequence).groupBy(identity).exists(_._2.size >= 2)
  }

  case object 七対子 extends Yaku {
    val han: Int = 2
    lazy val isDefined: Boolean = isClosed && closedMelds.forall(_.isPair)
  }

  case object 混全帯么九 extends Yaku {
    val han: Int = 喰い下がり(2)
    lazy val isDefined: Boolean = !純全帯么九.isDefined && melds.forall(_.isOrphan)
  }

  case object 対々和 extends Yaku {
    val han: Int = 2
    lazy val isDefined: Boolean = melds.count(_.isTriplet) == 4
  }

  case object 一気通貫 extends Yaku {
    val han: Int = 喰い下がり(2)
    lazy val isDefined: Boolean = Suit.values.exists(suit => (1 to 9 by 3).forall(n => find(Num(suit, n)).exists(_.isSequence)))
  }

  case object 三暗刻 extends Yaku {
    val han: Int = 2
    lazy val isDefined: Boolean = closedMelds.count(_.isTriplet) == 3
  }

  case object 三槓子 extends Yaku {
    val han: Int = 2
    lazy val isDefined: Boolean = melds.count(_.isQuad) == 3
  }

  case object 三色同刻 extends Yaku {
    val han: Int = 2
    lazy val isDefined: Boolean = (1 to 9).exists(n => Suit.values.forall(suit => find(Num(suit, n)).exists(_.isTriplet)))
  }

  case object 三色同順 extends Yaku {
    val han: Int = 喰い下がり(2)
    lazy val isDefined: Boolean = (1 to 9 by 3).exists(n => Suit.values.forall(suit => find(Num(suit, n)).exists(_.isSequence)))
  }

  case object 混老頭 extends Yaku {
    val han: Int = 2
    lazy val isDefined: Boolean = tiles.forall(_.isOrphan)
  }

  case object 小三元 extends Yaku {
    val han: Int = 2
    lazy val isDefined: Boolean = melds.count(_.tile.isDragon) == 3 && !七対子.isDefined
  }

  case object 二盃口 extends Yaku {
    val han: Int = 3
    lazy val isDefined: Boolean = closedMelds.filter(_.isSequence).groupBy(identity).count(_._2.size >= 2) == 2
  }

  case object 純全帯么九 extends Yaku {
    val han: Int = 喰い下がり(3)
    lazy val isDefined: Boolean = melds.forall(_.isTerminal)
  }

  case object 混一色 extends Yaku {
    val han: Int = 喰い下がり(3)
    lazy val isDefined: Boolean = !清一色.isDefined && Suit.values.exists(suit => tiles.forall(tile => (1 to 9).exists(n => Num(suit, n) == tile) || tile.isHonor))
  }

  case object 清一色 extends Yaku {
    val han: Int = 喰い下がり(6)
    lazy val isDefined: Boolean = Suit.values.exists(suit => tiles.forall(tile => (1 to 9).exists(n => Num(suit, n) == tile)))
  }

  case object 国士無双 extends Yaku {
    val han: Int = 13
    lazy val isDefined: Boolean = (Wind.values ++ Dragon.values ++ Vector(Num(Wan, 1), Num(Wan, 9), Num(Pin, 1), Num(Pin, 9), Num(Sou, 1), Num(Sou, 9))).forall(tiles.contains)
  }

  case object 四暗刻 extends Yaku {
    val han: Int = 13
    lazy val isDefined: Boolean = closedMelds.count(_.isTriplet) == 4
  }

  case object 大三元 extends Yaku {
    val han: Int = 13
    lazy val isDefined: Boolean = 白.isDefined && 發.isDefined && 中.isDefined
  }

  case object 字一色 extends Yaku {
    val han: Int = 13
    lazy val isDefined: Boolean = tiles.forall(_.isHonor)
  }

  case object 小四喜 extends Yaku {
    val han: Int = 13
    lazy val isDefined: Boolean = melds.count(_.tile.isWind) == 4 && !七対子.isDefined
  }

  case object 大四喜 extends Yaku {
    val han: Int = 13
    lazy val isDefined: Boolean = Wind.values.forall(wind => melds.exists(meld => meld.isTriplet && meld.tile == wind))
  }

  case object 緑一色 extends Yaku {
    val han: Int = 13
    lazy val isDefined: Boolean = tiles.forall(_.isGreen)
  }

  case object 清老頭 extends Yaku {
    val han: Int = 13
    lazy val isDefined: Boolean = tiles.forall(_.isTerminal)
  }

  case object 四槓子 extends Yaku {
    val han: Int = 13
    lazy val isDefined: Boolean = melds.forall(_.isQuad)
  }

  case object 九蓮宝燈 extends Yaku {
    val han: Int = 13
    lazy val isDefined: Boolean =
      Suit.values.exists { suit =>
        val diff = tiles.diff(Vector.fill(3)(Num(suit, 1)) ++ (2 to 8).map(Num(suit, _)) ++ Vector.fill(3)(Num(suit, 9)))
        (1 to 9).map(Num(suit, _)).exists(num => Vector(num) == diff)
      }
  }

}
