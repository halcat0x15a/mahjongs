package mahjongs

sealed abstract class 和了(val value: Int)

case class 親自摸和(nondealer: Int) extends 和了(nondealer * 3)

case class 子自摸和(dealer: Int, nondealer: Int) extends 和了(dealer + nondealer * 2)

case class 栄和(override val value: Int) extends 和了(value)
