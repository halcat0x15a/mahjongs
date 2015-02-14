package mahjongs

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{Reader, NoPosition}

object Mahjongs extends Parsers {
  type Elem = Tile
  def apply(tile: Tile, concealed: Seq[Tile], melded: Seq[Tile], selfpick: Boolean, dealer: Boolean, addition: Int) = {
    val scores = for {
      c <- parse((tile +: concealed).sorted)
      m <- parse(melded.sorted)
      _ = println(m, c)
      if c.size + m.size == 5
      w <- Wait.find(tile, c).toSeq
      _ = println(w)
    } yield {
      val hands = Hand.values.map(hands => hands find { hand =>
        hand.value match {
          case _: Closed => m.isEmpty && hand(c, w)
          case _: Open => hand(c ++ m, w)
          case _ => false
        }
      }).flatten
      println(hands)
      val han = hands.map(_.value match {
        case Closed(value) => value
        case Open(value, decrease) => if (decrease) value - 1 else value
      }).sum + (if (selfpick) 1 else 0) + addition
      println(han)
      val fu =
        if (selfpick && hands.contains(AllRuns)) 20
        else if (hands.contains(SevenPairs)) 25
        else if (AllRuns(m ++ c, w)) 30
        else Math.ceil((20 + (if (m.isEmpty) (if (selfpick) 2 else 10) else 0) + w.fu + c.map(_.fu).sum + m.map(_.fu / 2).sum).toDouble / 10) * 10
      val base =
        if (han >= 13) 8000
        else if (han >= 11) 6000
        else if (han >= 8) 4000
        else if (han >= 6) 3000
        else if (han >= 5) 2000
        else fu * Math.pow(2, han + 2)
      val score: Seq[Double] =
        if (dealer) {
          if (selfpick)
            Seq(base * 2)
          else
            Seq(base * 6)
        } else {
          if (selfpick)
            Seq(base, base * 2)
          else
            Seq(base * 4)
        }
      score.map(n => (Math.ceil(n / 100) * 100).toInt)
    }
    scores
  }
  def parse(tiles: Seq[Tile]) = hand(reader(tiles)).getOrElse(Nil)
  def reader(tiles: Seq[Tile]): Reader[Tile] =
    new Reader[Tile] {
      def first = tiles.head
      def rest = reader(tiles.tail)
      def atEnd = tiles.isEmpty
      def pos = NoPosition
    }
  def hand: Parser[Seq[Seq[Meld]]] =
    rep(
      seq(1) ^^ (tiles => Seq(Seq(Run(tiles)))) |
      complex(2) |
      complex(3) |
      run(2) ^^ (runs => Seq(runs)) |
      run(3) ^^ (runs => Seq(runs)) |
      set(3) ^^ (set => Seq(Seq(set))) |
      set(2) ^^ (set => Seq(Seq(set)))
    ) ^^ (patterns => sequence(patterns).map(_.flatten))
  def complex(n: Int): Parser[Seq[Seq[Meld]]] =
    for {
      tiles <- seq(n)
      sets = tiles.sliding(n, n).toList
    } yield Seq(0 until n map (i => Run(sets.map(_(i)))), sets.map(Set.apply))
  def set(n: Int): Parser[Set] =
    for {
      tiles@(head :: _) <- repN(n, tile)
      if tiles.distinct.size == 1
    } yield Set(tiles)
  def seq(n: Int): Parser[Seq[Number]] =
    for {
      numbers@(Number(suit, value) :: _) <- repN(3 * n, number)
      if numbers.forall(_.suit == suit) && numbers.map(_.value - value) == Seq(0, 1, 2).flatMap(x => Seq.fill(n)(x))
    } yield numbers
  def run(n: Int): Parser[Seq[Run]] =
    for {
      numbers@(Number(suit, value) :: _) <- repN(3 * n, number)
      if numbers.forall(_.suit == suit) && numbers.map(_.value - value) == (0 until n flatMap (i => 0 until 3 map (_ + i))).sorted
    } yield 0 until n map (j => Run(0 until 3 map (i => numbers(i * n + j))))
  def number: Parser[Number] =
    tile ^? {
      case number: Number => number
    }
  def tile: Parser[Tile] =
    Parser { input =>
      if (input.atEnd)
        Failure("end of input", input)
      else
        Success(input.first, input.rest)
    }
}
