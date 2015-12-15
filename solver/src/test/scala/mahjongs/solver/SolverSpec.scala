package mahjongs.solver

import org.scalacheck._

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

class SolverSpec extends FunSuite with Checkers {

  val tile = Gen.oneOf(Tile.values)

  val pair = tile.map(Toitsu)

  val triplet = for {
    tile <- tile
    isClosed <- Arbitrary.arbitrary[Boolean]
  } yield Kotsu(tile, isClosed)

  val sequence = for {
    n <- Gen.choose(1, 7)
    suit <- Gen.oneOf(Suit.values)
    isClosed <- Arbitrary.arbitrary[Boolean]
  } yield Shuntsu(Num(suit, n), isClosed)

  val meld = Gen.oneOf(triplet, sequence)

  val hand = for {
    pair <- pair
    melds <- Gen.listOfN(4, meld)
  } yield pair :: melds

  test("A parser should parse a meld") {
    check(Prop.forAll(meld)(meld => Meld.parse(meld.tiles, true).isDefined))
  }

  test("A parser should parse a hand") {
    check(Prop.forAllNoShrink(hand)(hand => Hand.parse(Hand.histgram(hand.flatMap(_.tiles))).exists(_.size == 5)))
  }

}
