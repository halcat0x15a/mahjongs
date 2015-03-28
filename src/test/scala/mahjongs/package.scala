import org.scalacheck.Gen

package object mahjongs {
  val tile = Gen.oneOf(Tile.values)
  val wind = Gen.oneOf(Wind.values)
  val seq = for {
    suit <- Gen.oneOf(Suit.values)
    start <- Gen.oneOf(1 to 7)
  } yield Meld.Seq(suit, start)
  val pair = tile.map(Meld.Pair)
  val triplet = tile.map(Meld.Triplet)
  val quad = tile.map(Meld.Quad)
  val meld = Gen.oneOf(seq, triplet, quad)
  val waiting = Gen.oneOf(Wait.Sides, Wait.Single, Wait.Double, Wait.Closed, Wait.Edge)
  val winning = Gen.oneOf(Drawn, Discard)
  val situation =
    for {
      winning <- winning
      dealer <- Gen.oneOf(true, false)
      player <- wind
      prevailing <- wind
    } yield Situation(winning, dealer, player, prevailing)
  val hand =
    for {
      pair <- pair
      melds <- Gen.listOfN(4, Gen.zip(Gen.oneOf(true, false), meld)).map(_.groupBy(_._1).mapValues(_.map(_._2)))
      concealed = melds.get(true).getOrElse(Nil)
      melded = melds.get(false).getOrElse(Nil)
      if (pair :: concealed ::: melded).flatMap(_.tiles).groupBy(identity).values.forall(_.size <= 4)
      wait <- waiting
      situation <- situation
    } yield Hand(pair +: concealed, melded, wait)(situation)
}
