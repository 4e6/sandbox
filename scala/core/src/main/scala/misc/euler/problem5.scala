package misc.euler

import misc.utils.NumericUtils._

trait Problem5 {

  def problem5 = {
    val numbers3 = 100 to 999
    val ps = for {
      x ← numbers3.par
      y ← numbers3.par
      if isPalindrome(digitsInt(x * y))
    } yield x * y

    ps.seq.sorted.last
  }
}
