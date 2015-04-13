package mahjongs

sealed abstract class 和了 {
  def value: Int
}

case class 自摸和(values: List[Int]) extends 和了 {
  def value = values.sum
}

case class 栄和(value: Int) extends 和了
