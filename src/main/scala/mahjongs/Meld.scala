package mahjongs

sealed trait Meld {
  def tiles: Seq[Tile]
  def fu: Int =
    this match {
      case Set(tiles@(tile +: _)) if tiles.size == 3 =>
        if (tile.isTerminal || tile.isHonor) 8 else 4
      case Set(tiles@(tile +: _)) if tiles.size == 4 =>
        if (tile.isTerminal || tile.isHonor) 32 else 16
      case Set(tiles@(tile +: _)) if tiles.size == 4 && (tile.isTerminal || tile.isHonor) => 2
      case _ => 0
    }
}

case class Set(tiles: Seq[Tile]) extends Meld

case class Run(tiles: Seq[Tile]) extends Meld
