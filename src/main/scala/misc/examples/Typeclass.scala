package misc.examples

case class Object1(field: String)
case class Object2(fields: List[String])

trait CanBeMasked[T] {
  def mask(o: T, f: String ⇒ String): T
}

object CanBeMasked {
  implicit object O1 extends CanBeMasked[Object1] {
    def mask(o: Object1, f: String ⇒ String) = o.copy(field = f(o.field))
  }
  implicit object O2 extends CanBeMasked[Object2] {
    def mask(o: Object2, f: String ⇒ String) = o.copy(fields = o.fields.map(f))
  }
}

trait MaskHelper {
  def masked[T: CanBeMasked](o: T, f: String ⇒ String): T = implicitly[CanBeMasked[T]].mask(o, f)
}
