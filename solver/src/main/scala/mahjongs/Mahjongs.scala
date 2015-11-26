package mahjongs

case class Mahjongs(
  winningTile: Tile,
  closedTiles: List[Tile],
  openTiles: List[(Boolean, List[Tile])],
  isDealer: Boolean,
  isSelfDrawn: Boolean,
  isLast: Boolean,
  seatWind: Wind,
  prevailingWind: Wind,
  dora: Int
) extends MahjongApi with HandApi {

  lazy val openMelds: List[Meld] =
    openTiles.flatMap {
      case (closed, tiles) =>
        if (closed)
          ClosedMelds.parse(tiles).toList
        else
          OpenMelds.parse(tiles).toList
    }

}

trait MahjongApi {

  val isDealer: Boolean

  val isSelfDrawn: Boolean

  val isLast: Boolean

  val seatWind: Wind

  val prevailingWind: Wind

  val dora: Int

  lazy val isDiscard: Boolean = !isSelfDrawn

}
