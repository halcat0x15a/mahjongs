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

  def calc(fu: Int, han: Int, situation: Situation): Win =
    calc(basicPoints(fu, han), situation)

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

  def basicPoints(fu: Int, han: Int): Int =
    if (han >= 13)
      8000
    else if (han >= 11)
      6000
    else if (han >= 8)
      4000
    else if (han >= 6)
      3000
    else if (han >= 5)
      2000
    else
      math.min(fu * math.pow(2, han + 2).toInt, 2000)

  def ceil(points: Double): Int =
    (math.ceil(points / 100) * 100).toInt

}
