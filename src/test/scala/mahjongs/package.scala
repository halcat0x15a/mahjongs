import org.scalacheck.Gen

package object mahjongs {
  val tile = Gen.oneOf(牌.values)
  val wind = Gen.oneOf(風牌.values)
  val seq = for {
    suit <- Gen.oneOf(組.values)
    i <- Gen.oneOf(0 until 7)
  } yield 順子(suit.values(i))
  val pair = tile.map(対子)
  val triplet = tile.map(刻子)
  val quad = tile.map(槓子)
  val meld = Gen.oneOf(seq, triplet, quad)
  val waiting = Gen.oneOf(聴牌.values)
  val situation =
    for {
      dealer <- Gen.oneOf(true, false)
      winning <- Gen.oneOf(true, false)
      prevailing <- wind
      player <- wind
      dora <- Gen.choose(0, 40)
    } yield 状況(winning, dealer, player, prevailing, dora)
  val hand =
    for {
      pair <- pair
      melds <- Gen.listOfN(4, Gen.zip(Gen.oneOf(true, false), meld)).map(_.groupBy(_._1).mapValues(_.map(_._2)))
      concealed = melds.get(true).getOrElse(Nil)
      melded = melds.get(false).getOrElse(Nil)
      if (pair :: concealed ::: melded).flatMap(_.tiles).groupBy(identity).values.forall(_.size <= 4)
      wait <- waiting
      situation <- situation
    } yield 手牌(pair +: concealed, melded, wait)(situation)
}
