package mahjongs.solver

case class Situation(isDealer: Boolean, isSelfDrawn: Boolean, seatWind: Wind, roundWind: Wind, dora: Int) {

  def isHonor(tile: Tile): Boolean = tile.isDragon || tile == seatWind || tile == roundWind

  def isDoubleWind(tile: Tile): Boolean = tile == seatWind && tile == roundWind

  def fu(isClosed: Boolean): Int =
    if (isClosed) {
      if (isSelfDrawn)
        2
      else
        10
    } else {
      0
    }

}
