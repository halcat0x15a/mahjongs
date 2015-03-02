package mahjongs

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{Reader, NoPosition}

object Mahjongs extends Parsers {
  case class Result(winning: Winning, han: Int, fu: Int, hands: Seq[Hand], melds: Seq[Meld])
  type Elem = Tile
  def ceil(value: Double, n: Int): Double = math.ceil(value / n) * n
  def apply(tile: Tile, concealed: Seq[Tile], quads: Seq[Meld.Quad], melded: Seq[Meld], selfpick: Boolean, dealer: Boolean, prevailing: Wind, player: Wind, addition: Int) = {
    val tiles = ((tile -> true) +: concealed.map(_ -> false)).sorted
    val scores = for {
      melds <- parse(tiles.map(_._1))
      if melds.size + quads.size + melded.size == 5
      wait <- Wait.find(tiles.takeWhile(_._2).size, melds).toSeq
    } yield {
      val hands = Hand.values(prevailing, player).map(hands => hands.find(_.check(melds ++ quads, melded))).flatten.filter(Hand.check(selfpick, wait))
      val han = hands.map(hand => if (hand.decrease && melded.nonEmpty) hand.value - 1 else hand.value).sum + addition
      val fu =
        if (selfpick && hands.contains(Hand.AllSeqs)) 20
        else if (hands.contains(Hand.SevenPairs)) 25
        else if (Hand.AllSeqs.check(melds ++ quads, melded) && wait == Wait.Sides) 30
        else ceil((20 + (if (melded.isEmpty) (if (selfpick) 2 else 10) else 0) + wait.fu + (melds ++ quads).map(_.fu).sum + melded.map(_.fu / 2).sum).toDouble, 10).toInt
      val base =
        if (han >= 13) 8000
        else if (han >= 11) 6000
        else if (han >= 8) 4000
        else if (han >= 6) 3000
        else if (han >= 5) 2000
        else math.min(fu * Math.pow(2, han + 2), 2000)
      val winning =
        if (dealer) {
          if (selfpick)
            DealerDrawn(ceil(base * 2, 100).toInt)
          else
            Discard(ceil(base * 6, 100).toInt)
        } else {
          if (selfpick)
            NonDealerDrawn(ceil(base, 100).toInt, ceil(base * 2, 100).toInt)
          else
            Discard(ceil(base * 4, 100).toInt)
        }
      Result(winning, han, fu, hands, melds ++ quads ++ melded)
    }
    if (scores.nonEmpty)
      Some(scores.maxBy(_.winning.value))
    else
      None
  }
  def parse(tiles: Seq[Tile]) = hand(reader(tiles)).getOrElse(Nil)
  def reader(tiles: Seq[Tile]): Reader[Tile] =
    new Reader[Tile] {
      def first = tiles.head
      def rest = reader(tiles.tail)
      def atEnd = tiles.isEmpty
      def pos = NoPosition
    }
  def tile: Parser[Tile] =
    Parser { input =>
      if (input.atEnd)
        Failure("end of input", input)
      else
        Success(input.first, input.rest)
    }
  def number: Parser[Number] =
    tile ^? {
      case number: Number => number
    }
  def set(n: Int): Parser[Seq[Seq[Meld.Set]]] =
    for {
      tiles@(tile :: _) <- repN(n, tile)
      if tiles.distinct.size == 1
    } yield List(List(Meld.Set(tile, n)))
  def complex(n: Int): Parser[Seq[Seq[Meld]]] =
    for {
      numbers@(Number(suit, value) :: _) <- repN(3 * n, number)
      if numbers.forall(_.suit == suit) && numbers.map(_.value - value) == Seq(0, 1, 2).flatMap(i => Seq.fill(n)(i))
    } yield Seq(Seq.fill(n)(Meld.Seq(suit, value)), numbers.distinct.map(Meld.Set(_, n)))
  def seq(n: Int): Parser[Seq[Seq[Meld.Seq]]] =
    for {
      numbers@(Number(suit, value) :: _) <- repN(3 * n, number)
      if numbers.forall(_.suit == suit) && numbers.map(_.value - value) == (0 until n flatMap (i => 0 until 3 map (_ + i))).sorted
    } yield Seq(for (i <- 0 until n) yield Meld.Seq(suit, value + i))
  def hand: Parser[Seq[Seq[Meld]]] =
    rep(seq(3) | seq(2) | seq(1) | complex(3) | complex(2) | set(3) | set(2)) ^^ (patterns => sequence(patterns).map(_.flatten))
}
