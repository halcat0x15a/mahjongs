package mahjongs

import org.scalacheck.Properties
import org.scalacheck.Prop._

object MeldSpecification extends Properties("Meld") {
  property("parse") = forAll(meld) { meld =>
    Meld.parse(meld.tiles).isDefined
  }
}
