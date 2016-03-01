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
        DealerTsumo(ceil(basicPoints * 2, 100))
      else
        NonDealerTsumo(ceil(basicPoints * 2, 100), ceil(basicPoints, 100))
    } else {
      if (situation.isDealer)
        Ron(ceil(basicPoints * 6, 100))
      else
        Ron(ceil(basicPoints * 4, 100))
    }

}
