package mahjongs

import org.scalacheck._

object MahjongSpec extends Properties("Mahjongs") {

  def tile = Gen.oneOf(Tile.values)

  def pair = tile.map(Pair)

  def triplet = tile.flatMap(tile => Gen.frequency(2 -> OpenMelds.Triplet(tile), 1 -> ClosedMelds.Triplet(tile)))

  def quad = tile.flatMap(tile => Gen.frequency(2 -> OpenMelds.Quad(tile), 1 -> ClosedMelds.Quad(tile)))

  def sequence = Gen.oneOf(Num.values).flatMap(tile => Gen.frequency(2 -> OpenMelds.Sequence(tile), 1 -> ClosedMelds.Sequence(tile)))

  def meld = Gen.frequency(3 -> sequence, 2 -> triplet, 1 -> quad)

  def melds = for {
    pair <- pair
    melds <- Gen.listOfN(4, meld)
  } yield pair :: melds

  def pairs = Gen.listOfN(7, pair)

  def hand = Gen.frequency(50 -> melds, 1 -> pairs)

  property("parse") = Prop.forAll(hand) { melds =>
    val (closedMelds, openMelds) = melds.partition(_.isClosed)
    closedMelds.flatMap(_.tiles) match {
      case winningTile :: closedTiles => Mahjongs(winningTile, closedTiles, openMelds, true, true, true, East, East, 0).hands.nonEmpty
      case Nil => false
    }
  }

}
