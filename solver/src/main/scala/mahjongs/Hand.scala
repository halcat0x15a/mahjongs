package mahjongs

sealed abstract case class Hand(closedMelds: List[Meld], openMelds: List[Meld], waiting: Wait) {

  def fuList: List[Fu]

  def yakuList: List[Yaku]

  def score: Win

  lazy val fu: Int = ceil(fuList.map(_.value).sum, 10)

  lazy val han: Int = yakuList.map(_.han).sum

  lazy val basicPoint: Int =
    if (han >= 13) 8000
    else if (han >= 11) 6000
    else if (han >= 8) 4000
    else if (han >= 6) 3000
    else if (han >= 5) 2000
    else math.min(fu * math.pow(2, han + 2).toInt, 2000)

  def ceil(value: Double, n: Int): Int = (math.ceil(value / n) * n).toInt

}

trait HandApi { self: MahjongApi =>

  def winningTile: Tile

  def closedTiles: List[Tile]

  def openMelds: List[Meld]

  def Hand(closedMelds: List[Meld], openMelds: List[Meld], waiting: Wait): Hand =
    new Hand(closedMelds, openMelds, waiting) with FuApi with YakuApi {

      val mahjong: MahjongApi = self

      lazy val fuList: List[Fu] =
        if (門前清自摸和.isDefined && 平和.isDefined)
          List(Fu.ツモ平和)
        else if (七対子.isDefined)
          List(Fu.七対子)
        else if (isOpen && isNoPointsHand)
          List(Fu.喰い平和)
        else if (isDiscard && isClosed)
          Fu.副底 :: Fu.門前加符 :: fu(waiting) :: melds.map(fu)
        else if (isSelfDrawn)
          Fu.副底 :: Fu.自摸符 :: fu(waiting) :: melds.map(fu)
        else
          Fu.副底 :: fu(waiting) :: melds.map(fu)

      lazy val yakuList: List[Yaku] = allYaku.filter(_.isDefined)

      lazy val score: Win =
        if (isSelfDrawn && isDealer)
          DealerSelfDrawn(ceil(basicPoint * 2, 100))
        else if (isSelfDrawn)
          SelfDrawn(ceil(basicPoint * 2, 100), ceil(basicPoint, 100))
        else if (isDealer)
          Discard(ceil(basicPoint * 6, 100))
        else
          Discard(ceil(basicPoint * 4, 100))

    }

  lazy val hands: List[Hand] =
    for {
      melds <- combinations(winningTile :: closedTiles)
      if isValid(melds ::: openMelds)
      meld <- melds.headOption
      wait <- Wait.waiting(winningTile, meld)
    } yield Hand(melds, openMelds, wait)

  lazy val hand: Option[Hand] =
    if (hands.nonEmpty)
      Some(hands.maxBy(_.score.point))
    else
      None

  def combinations(tiles: List[Tile]): List[List[Meld]] =
    tiles match {
      case tile :: tiles =>
        val sequences: List[List[Meld]] = tile match {
          case num: Num =>
            val sequence = ClosedMelds.Sequence(num)
            val tail = sequence.tiles.tail
            if (tail.forall(tiles.contains))
              combinations(tiles diff tail).map(sequence :: _)
            else
              Nil
          case _ => Nil
        }
        val triplets: List[List[Meld]] = for {
          n <- (0 until tiles.count(_ == tile)).toList
          melds <- combinations(tiles diff List.fill(n + 1)(tile))
          meld <- n match {
            case 0 => Some(Pair(tile))
            case 1 => Some(ClosedMelds.Triplet(tile))
            case _ => None
          }
        } yield meld :: melds
        sequences ::: triplets
      case _ => List(Nil)
    }

  def isValid(melds: List[Meld]) =
    melds.count(_.isPair) == 1 && melds.size == 5 || melds.count(_.isPair) == 7

}

object Hand {

  def combinations(tiles: List[Tile]): List[List[Meld]] =
    tiles match {
      case tile :: tiles =>
        val sequences: List[List[Meld]] = tile match {
          case num: Num =>
            val sequence = ClosedMelds.Sequence(num)
            val tail = sequence.tiles.tail
            if (tail.forall(tiles.contains))
              combinations(tiles diff tail).map(sequence :: _)
            else
              Nil
          case _ => Nil
        }
        val triplets: List[List[Meld]] = for {
          n <- (0 until tiles.count(_ == tile)).toList
          melds <- combinations(tiles diff List.fill(n + 1)(tile))
          meld <- n match {
            case 0 => Some(Pair(tile))
            case 1 => Some(ClosedMelds.Triplet(tile))
            case _ => None
          }
        } yield meld :: melds
        sequences ::: triplets
      case _ => List(Nil)
    }

}
