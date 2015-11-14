package mahjongs

sealed trait Score {

  def point: Int

}

case class Discard(point: Int) extends Score

case class SelfDrawn(dealer: Int, others: Int) extends Score {

  lazy val point = dealer + others * 2

}

case class DealerSelfDrawn(others: Int) extends Score {

  lazy val point = others * 3

}
