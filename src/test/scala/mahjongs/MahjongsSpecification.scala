package mahjongs

import org.scalacheck.{Properties, Gen}
import org.scalacheck.Prop._

object MahjongsSpecification extends Properties("Mahjongs") {
  val tile = Gen.oneOf(Tile.values)
  val seq = for {
    suit <- Gen.oneOf(Suit.values)
    start <- Gen.oneOf(1 to 7)
  } yield Meld.Seq(suit, start)
  val pair = tile.map(Meld.Pair)
  val triplet = tile.map(Meld.Triplet)
  val quad = tile.map(Meld.Quad)
  val meld = Gen.oneOf(seq, triplet, quad)
  val hand =
    for {
      pair <- pair
      melds <- Gen.listOfN(4, Gen.zip(Gen.oneOf(true, false), meld)).map(_.groupBy(_._1).mapValues(_.map(_._2)))
      concealed = melds.get(true).getOrElse(Nil)
      melded = melds.get(false).getOrElse(Nil)
      if (pair :: concealed ::: melded).flatMap(_.tiles).groupBy(identity).values.forall(_.size <= 4)
      tiles = (pair :: concealed).filterNot(_.isInstanceOf[Meld.Quad]).flatMap(_.tiles)
    } yield Hand(tiles.head, tiles.tail, concealed.collect { case quad: Meld.Quad => quad }, melded)
  property("combinations") = forAll(hand) { hand =>
    Mahjongs.combinations(hand.tile +: hand.concealed).forall(_.size > 0)
  }
}
