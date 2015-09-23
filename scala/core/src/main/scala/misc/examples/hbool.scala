package misc.examples.bool

import shapeless._

trait Bool {
  type _B <: Bool
}
object Bool {
  val True = new True
  val False = new False
}

class True extends Bool { type _B = True }
class False extends Bool { type _B = False }

trait OR[A, B] extends Bool
object OR {
  def or = ???
}
