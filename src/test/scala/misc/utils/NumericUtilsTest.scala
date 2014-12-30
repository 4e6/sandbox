package misc.utils

import org.scalacheck.{ Arbitrary, Gen, Properties }
import org.scalacheck.Prop._

class NumericUtilsTest extends Properties("NumericUtils") with NumericUtils {

  val positiveInt = Gen.chooseNum(0, Int.MaxValue)
  val digit = Gen.choose(1, 9)
  val intPoly = for {
    x ← digit
    xs ← Gen.listOfN(x, digit)
  } yield xs

  def naiveDigits[T](x: T): List[Int] =
    x.toString.filter(_.isDigit).map(_.toString.toInt).toList

  property("isEven[BigInt]") = forAll { x: BigInt ⇒ (x % 2 == 0) ==> isEven(x) }
  property("isEven[Int]") = forAll { x: Int ⇒ (x % 2 == 0) ==> isEven(x) }
  property("isEven[Long]") = forAll { x: Long ⇒ (x % 2 == 0) ==> isEven(x) }
  property("isEven[Short]") = forAll { x: Short ⇒ (x % 2 == 0) ==> isEven(x) }
  property("isEven[Byte]") = forAll { x: Byte ⇒ (x % 2 == 0) ==> isEven(x) }
  property("isEven[Char]") = forAll { x: Char ⇒ (x % 2 == 0) ==> isEven(x) }

  property("digitsInt") = forAll { x: Int ⇒ digitsInt(x) == naiveDigits(x) }

  property("digits[BigInt]") = forAll { x: BigInt ⇒ digits(x) == naiveDigits(x) }
  property("digits[Int]") = forAll { x: Int ⇒ digits(x) == naiveDigits(x) }
  property("digits[Long]") = forAll { x: Long ⇒ digits(x) == naiveDigits(x) }
  property("digits[Short]") = forAll { x: Short ⇒ digits(x) == naiveDigits(x) }
  property("digits[Byte]") = forAll { x: Byte ⇒ digits(x) == naiveDigits(x) }
  property("digits[Char]") = forAll { x: Char ⇒ digits(x) == naiveDigits(x.toInt) }
}
