package mahjongs

import org.scalacheck.Properties
import org.scalacheck.Prop._

object HandSpecification extends Properties("Hand") {
  property("combinations") = forAll(hand) { hand =>
    Hand.combinations(hand.concealed.filterNot(_.isInstanceOf[Meld.Quad]).flatMap(_.tiles)).forall(_.size > 0)
  }
}
