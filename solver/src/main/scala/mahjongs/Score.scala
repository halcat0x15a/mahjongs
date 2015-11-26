package mahjongs

sealed trait Win {

  def point: Int

}

case class Discard(point: Int) extends Win

case class SelfDrawn(dealer: Int, others: Int) extends Win {

  lazy val point = dealer + others * 2

}

case class DealerSelfDrawn(others: Int) extends Win {

  lazy val point = others * 3

}
