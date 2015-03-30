package mahjongs

sealed trait 牌 {
  def unicode: String
}

object 牌 {
  def values: List[牌] =
    萬子.values ::: 筒子.values ::: 索子.values ::: 風牌.values ::: 三元牌.values
  implicit def ordering: Ordering[牌] =
    new Ordering[牌] {
      def compare(x: 牌, y: 牌) =
        values.indexOf(x) compareTo values.indexOf(y)
    }
}

sealed trait 組 {
  def values: List[数牌]
}
object 組 {
  def values = List(萬子, 筒子, 索子)
}

sealed abstract class 数牌(val suit: 組) extends 牌 {
  def index: Int = suit.values.indexOf(this)
  def sequence: List[数牌] = suit.values.slice(index, index + 3)
}
object 数牌 {
  def unapply(tile: 数牌): Option[Int] = Some(tile.index + 1)
}

sealed trait 么九牌 extends 牌

sealed trait 老頭牌 extends 么九牌
sealed trait 中張牌 extends 数牌

sealed abstract class 萬子(val unicode: String) extends 数牌(萬子)
case object 萬子 extends 組 {
  def values = List(一萬, 二萬, 三萬, 四萬, 五萬, 六萬, 七萬, 八萬, 九萬)
}

case object 一萬 extends 萬子("\ud83c\udc07") with 老頭牌
case object 二萬 extends 萬子("\ud83c\udc08") with 中張牌
case object 三萬 extends 萬子("\ud83c\udc09") with 中張牌
case object 四萬 extends 萬子("\ud83c\udc0a") with 中張牌
case object 五萬 extends 萬子("\ud83c\udc0b") with 中張牌
case object 六萬 extends 萬子("\ud83c\udc0c") with 中張牌
case object 七萬 extends 萬子("\ud83c\udc0d") with 中張牌
case object 八萬 extends 萬子("\ud83c\udc0e") with 中張牌
case object 九萬 extends 萬子("\ud83c\udc0f") with 老頭牌

sealed abstract class 筒子(val unicode: String) extends 数牌(筒子)
case object 筒子 extends 組 {
  def values = List(一筒, 二筒, 三筒, 四筒, 五筒, 六筒, 七筒, 八筒, 九筒)
}

case object 一筒 extends 筒子("\ud83c\udc19") with 老頭牌
case object 二筒 extends 筒子("\ud83c\udc1a") with 中張牌
case object 三筒 extends 筒子("\ud83c\udc1b") with 中張牌
case object 四筒 extends 筒子("\ud83c\udc1c") with 中張牌
case object 五筒 extends 筒子("\ud83c\udc1d") with 中張牌
case object 六筒 extends 筒子("\ud83c\udc1e") with 中張牌
case object 七筒 extends 筒子("\ud83c\udc1f") with 中張牌
case object 八筒 extends 筒子("\ud83c\udc20") with 中張牌
case object 九筒 extends 筒子("\ud83c\udc21") with 老頭牌

sealed abstract class 索子(val unicode: String) extends 数牌(索子)
case object 索子 extends 組 {
  def values = List(一索, 二索, 三索, 四索, 五索, 六索, 七索, 八索, 九索)
}

case object 一索 extends 索子("\ud83c\udc10") with 老頭牌
case object 二索 extends 索子("\ud83c\udc10") with 中張牌
case object 三索 extends 索子("\ud83c\udc10") with 中張牌
case object 四索 extends 索子("\ud83c\udc10") with 中張牌
case object 五索 extends 索子("\ud83c\udc10") with 中張牌
case object 六索 extends 索子("\ud83c\udc10") with 中張牌
case object 七索 extends 索子("\ud83c\udc10") with 中張牌
case object 八索 extends 索子("\ud83c\udc10") with 中張牌
case object 九索 extends 索子("\ud83c\udc10") with 老頭牌

sealed trait 字牌 extends 么九牌
object 字牌 {
  def values = 風牌.values ::: 三元牌.values
}

sealed abstract class 風牌(val unicode: String) extends 字牌
object 風牌 {
  def values = List(東, 南, 西, 北)
}

case object 東 extends 風牌("\ud83c\udc00")
case object 南 extends 風牌("\ud83c\udc01")
case object 西 extends 風牌("\ud83c\udc02")
case object 北 extends 風牌("\ud83c\udc03")

sealed abstract class 三元牌(val unicode: String) extends 字牌
object 三元牌 {
  def values = List(白, 發, 中)
}

case object 白 extends 三元牌("\ud83c\udc06")
case object 發 extends 三元牌("\ud83c\udc05")
case object 中 extends 三元牌("\ud83c\udc04")
