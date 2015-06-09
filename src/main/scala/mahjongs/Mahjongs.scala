package mahjongs

case class Mahjongs(
  winningTile: Tile,
  closedTiles: List[Tile],
  openMelds: List[Meld],
  isDealer: Boolean,
  isSelfDrawn: Boolean,
  isLast: Boolean,
  seatWind: Wind,
  prevailingWind: Wind,
  dora: Int
) extends MahjongApi with HandApi

trait MahjongApi {

  val isDealer: Boolean

  val isSelfDrawn: Boolean

  val isLast: Boolean

  val seatWind: Wind

  val prevailingWind: Wind

  val dora: Int

  lazy val isDiscard: Boolean = !isSelfDrawn

}
