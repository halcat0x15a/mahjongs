package mahjongs

sealed trait Score {
  def value: Int
}

case class DealerDrawn(nondealer: Int) extends Score {
  def value = nondealer * 3
}

case class NonDealerDrawn(dealer: Int, nondealer: Int) extends Score {
  def value = dealer + nondealer * 2
}

case class Discard(value: Int) extends Score
