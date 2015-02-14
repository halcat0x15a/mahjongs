package mahjongs

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{Reader, NoPosition}

object Mahjongs {
  def apply(tile: Tile, concealed: Seq[Tile], melded: Seq[Tile]) =
    for {
      c <- Parser((tile +: concealed).sorted)
      w <- Wait.find(tile, c).toSeq
      m <- Parser(melded.sorted)
      if c.size + m.size == 5
    } yield (c, m, w)
}

object Parser extends Parsers {
  type Elem = Tile
  def apply(tiles: Seq[Tile]) = hands(reader(tiles)).getOrElse(Nil)
  def reader(tiles: Seq[Tile]): Reader[Tile] =
    new Reader[Tile] {
      def first = tiles.head
      def rest = reader(tiles.tail)
      def atEnd = tiles.isEmpty
      def pos = NoPosition
    }
  def hands: Parser[Seq[Seq[Meld]]] =
    rep(hand) ^^ (patterns => sequence(patterns).map(_.flatten))
  def hand: Parser[Seq[Seq[Meld]]] =
    seq(1) ^^ (tiles => Seq(Seq(Run(tiles)))) |
    complex(2) |
    complex(3) |
    set(3) ^^ (set => Seq(Seq(set))) |
    set(2) ^^ (set => Seq(Seq(set)))
  def complex(n: Int): Parser[Seq[Seq[Meld]]] =
    sets(n) ^^ (tiles => Seq(0 until n map (i => Run(tiles.map(_(i)))), tiles.map(Set.apply)))
  def sets(n: Int): Parser[Seq[Seq[Number]]] =
    seq(n) ^^ (_.sliding(n, n).toList)
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
