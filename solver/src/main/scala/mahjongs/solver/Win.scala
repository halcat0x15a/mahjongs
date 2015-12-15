package mahjongs.solver

sealed abstract class Win {

  def points: Int

}

case class Ron(points: Int) extends Win

case class NonDealerTsumo(dealer: Int, nonDealers: Int) extends Win {

  val points: Int = dealer + nonDealers * 2

}

case class DealerTsumo(nonDealers: Int) extends Win {

  val points: Int = nonDealers * 3

}

object Win {

  def calc(basicPoints: Int, situation: Situation): Win =
    if (situation.isSelfDrawn) {
      if (situation.isDealer)
        DealerTsumo(ceil(basicPoints * 2))
      else
        NonDealerTsumo(ceil(basicPoints * 2), ceil(basicPoints))
    } else {
      if (situation.isDealer)
        Ron(ceil(basicPoints * 6))
      else
        Ron(ceil(basicPoints * 4))
    }

  def ceil(points: Double): Int =
    (math.ceil(points / 100) * 100).toInt

}
