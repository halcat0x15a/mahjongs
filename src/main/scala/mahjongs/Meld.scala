package mahjongs

sealed trait Meld {
  def tiles: Seq[Tile]
}

case class Set(tiles: Seq[Tile]) extends Meld

case class Run(tiles: Seq[Tile]) extends Meld
