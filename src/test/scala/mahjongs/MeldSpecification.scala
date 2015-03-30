package mahjongs

import org.scalacheck.Properties
import org.scalacheck.Prop._

object MeldSpecification extends Properties("Meld") {
  property("parse") = forAll(meld) { meld =>
    面子.parse(meld.tiles).isDefined
  }
}
