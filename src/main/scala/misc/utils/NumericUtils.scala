package misc.utils

import scala.annotation.tailrec

trait NumericUtils {

  def isEvenInt(x: Int): Boolean = {
    x % 2 == 0
  }

  def isEvenT[T](x: T)(implicit int: Integral[T]): Boolean = {
    import int._
    x % fromInt(2) equiv zero
  }

  def isEven[@specialized T](x: T)(implicit int: Integral[T]): Boolean = {
    import int._
    x % fromInt(2) equiv zero
  }

  def isOddT[T: Integral](x: T) = !isEvenT(x)

  def isOdd[@specialized T: Integral](x: T) = !isEven(x)

  def digitsNaive(x: Int): List[Int] =
    x.toString.map(_.toString.toInt)(collection.breakOut)

  def digitsInt(x: Int): List[Int] = {
    @tailrec def loop(x: Int, ds: List[Int]): List[Int] =
      if (x == 0) if (ds.isEmpty) x :: ds else ds
      else loop(x / 10, (x % 10).abs :: ds)

    loop(x, Nil)
  }

  def digitsT[T](x: T)(implicit int: Integral[T]): List[Int] = {
    import int._
    val ten = fromInt(10) // doesn't impact at all
    @tailrec def loop(x: T, ds: List[Int]): List[Int] =
      if (x equiv zero) if (ds.isEmpty) x.toInt :: ds else ds
      else loop(x / ten, (x % ten).abs.toInt :: ds)

    loop(x, Nil)
  }

  def digits[@specialized T](x: T)(implicit int: Integral[T]): List[Int] = {
    import int._
    val ten = fromInt(10) // gives two times speed increase
    @tailrec def loop(x: T, ds: List[Int]): List[Int] =
      if (x equiv zero) if (ds.isEmpty) x.toInt :: ds else ds
      else loop(x / ten, (x % ten).abs.toInt :: ds)

    loop(x, Nil)
  }

  def poly(coeffs: Iterable[Int], x: Int = 10): Double =
    coeffs.zipWithIndex
      .map { case (digit, index) ⇒ digit * Math.pow(x, index) }
      .sum

  def isPalindrome[A](xs: Seq[A]): Boolean = {
    val size = xs.size
    val (left, rt) = xs.splitAt(size / 2)
    val right = if (isOdd(size)) rt.tail else rt

    left zip right.reverse forall { case (l, r) ⇒ l == r }
  }
}

object NumericUtils extends NumericUtils
