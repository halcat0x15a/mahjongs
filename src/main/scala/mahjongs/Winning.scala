package mahjongs

sealed trait Winning extends Any {
  def value: Int
}

case class NonDealerDrawn(dealer: Int, nondealer: Int) extends Winning {
  def value = dealer + nondealer * 2
}

case class DealerDrawn(nondealer: Int) extends AnyVal with Winning {
  def value = nondealer * 3
}

case class Discard(value: Int) extends AnyVal with Winning
