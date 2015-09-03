package misc.examples

import shapeless._
import shapeless.syntax.std.function._
import shapeless.ops.function._

object Shapeless {

  def applyProduct[P <: Product, F, L <: HList, R](p: P)(f: F)(implicit gen: Generic.Aux[P, L], fp: FnToProduct.Aux[F, L ⇒ R]) =
    f.toProduct(gen.to(p))
}
