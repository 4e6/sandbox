package misc.utils

import org.scalacheck.{ Gen, Properties }
import org.scalacheck.Prop._

import misc.utils.NumericUtils.poly

class PolyProperties extends Properties("poly") {

  val digit = Gen.choose(1, 9)
  val intPoly = for {
    x ← digit
    xs ← Gen.listOfN(x, digit)
  } yield xs

  property("size") = forAll(intPoly) { xs: List[Int] ⇒
    poly(xs).toInt.toString.size == xs.size
  }

  property("positive") = forAll(intPoly) { xs: List[Int] ⇒
    poly(xs) > 0
  }

  property("numbers") = forAll(intPoly) { xs: List[Int] ⇒
    poly(xs).toInt.toString zip xs.reverse forall {
      case (x, y) ⇒ x.toString.toInt == y
    }
  }
}
