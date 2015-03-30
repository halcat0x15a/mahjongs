package mahjongs

import org.scalacheck.Properties
import org.scalacheck.Prop._

object HandSpecification extends Properties("Hand") {
  property("combinations") = forAll(hand) { hand =>
    手牌.combinations(hand.concealed.filterNot(_.isInstanceOf[槓子]).flatMap(_.tiles)).forall(_.size > 0)
  }
}
