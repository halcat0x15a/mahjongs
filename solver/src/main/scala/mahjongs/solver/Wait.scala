package mahjongs.solver

sealed abstract class Wait(val name: String, val fu: Int)

case object Ryanmen extends Wait("両門", 0)

case object Shanpon extends Wait("双碰", 0)

case object Penchan extends Wait("辺張", 2)

case object Kanchan extends Wait("嵌張", 2)

case object Tanki extends Wait("単騎", 2)

object Wait {

  def calc(tile: Tile, meld: Meld): Option[Wait] = {
    val index = meld.tiles.indexOf(tile)
    if (index < 0) {
      None
    } else {
      meld match {
        case Toitsu(_) => Some(Tanki)
        case Kotsu(_, true) => Some(Shanpon)
        case Shuntsu(Num(_, _), true) if index == 1 => Some(Kanchan)
        case Shuntsu(Num(_, n), true) if n == 1 && index == 2 || n == 7 && index == 0 => Some(Penchan)
        case Shuntsu(Num(_, _), true) => Some(Ryanmen)
        case _ => None
      }
    }
  }

}
