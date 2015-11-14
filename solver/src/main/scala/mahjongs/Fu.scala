package mahjongs

sealed trait Fu {

  def value: Int

}

object Fu {

  case object 副底 extends Fu {
    val value: Int = 20
  }

  case object 門前加符 extends Fu {
    val value: Int = 10
  }

  case object 自摸符 extends Fu {
    val value: Int = 2
  }

  case object ツモ平和 extends Fu {
    val value: Int = 20
  }

  case object 七対子 extends Fu {
    val value: Int = 25
  }

  case object 喰い平和 extends Fu {
    val value: Int = 30
  }

}

trait FuApi {

  val mahjong: MahjongApi

  import mahjong._

  def fu(waiting: Wait): Fu =
    new Fu {
      lazy val value: Int =
        waiting match {
          case Wait.嵌張 | Wait.辺張 | Wait.単騎 => 2
          case _ => 0
        }
      override def toString: String = wait.toString
    }

  def fu(meld: Meld): Fu =
    new Fu {
      lazy val value: Int =
        meld match {
          case Triplet(tile) if meld.isClosed && tile.isOrphan => 8
          case Triplet(tile) if meld.isClosed || tile.isOrphan => 4
          case Triplet(tile) => 2
          case Quad(tile) if meld.isClosed && tile.isOrphan => 32
          case Quad(tile) if meld.isClosed || tile.isOrphan => 16
          case Quad(tile) => 8
          case Pair(tile) if tile == seatWind && tile == prevailingWind => 4
          case Pair(tile) if tile == seatWind || tile == prevailingWind || tile.isDragon => 2
          case _ => 0
        }
      override def toString: String = meld.toString
    }

}
