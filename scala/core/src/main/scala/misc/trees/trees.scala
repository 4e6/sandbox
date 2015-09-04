package misc.trees

import shapeless._
import scala.reflect._

// Types
trait Type {
  type TT
  def tag: ClassTag[TT]
  override def toString = tag.toString
}
object Type {
  type Aux[T] = Type { type TT = T }

  implicit def apply[A: ClassTag]: Aux[A] =
    new Type {
      type TT = A
      val tag = classTag[A]
    }
}

// Trees

sealed trait HTree {
  type T
  def tpe: Type.Aux[T]
}
object HTree {
  type Aux[T0] = HTree { type T = T0 }
}

case class Const[A](tpe: Type.Aux[A], value: A) extends HTree {
  type T = A
}
object Const {
  def apply[A: ClassTag](v: A): Const[A] =
    Const[A](Type[A], v)
}

case class Plus[A, LT <: HTree, RT <: HTree](tpe: Type.Aux[A], lexpr: LT, rexpr: RT) extends HTree {
  type T = A
}
object Plus {
  def apply[LT <: HTree, RT <: HTree]
    (lexpr: LT, rexpr: RT)
    (implicit
      ev: lexpr.T =:= rexpr.T
    ): Plus[lexpr.T, LT, RT] =
    Plus(lexpr.tpe, lexpr, rexpr)

  def ap[LT <: HTree, RT <: HTree, A]
    (lexpr: LT, rexpr: RT)
    (implicit
      ul: Unpack1[LT, HTree.Aux, A],
      ur: Unpack1[RT, HTree.Aux, A],
      tag: ClassTag[A]
    ): Plus[A, LT, RT] =
    Plus(Type[A], lexpr, rexpr)
}

// case class Lambda[A, HB <: HTree](tpe: Type.Aux[A => HB], f: A => HB) extends HTree {
//   type T = A => HB
//   type TT = HB#T
// }
trait Lambda extends HTree {

  type A
  type HB <: HTree

  type T = A => HB
  type TT = HB#T

  def f: A => HB
}
object Lambda {
  type Aux[A0, HB0 <: HTree] = Lambda { type A = A0; type HB = HB0 }
  def apply[HB0 <: HTree, A0]
    (f0: A0 => HB0)
    (implicit
      tA: ClassTag[A0],
      tB: ClassTag[HB0]
    ): Aux[A0, HB0] =
    new Lambda {
      type A = A0
      type HB = HB0

      def tpe = Type[A => HB]
      def f = f0
    }
}

case class Apply[HA <: HTree, L <: Lambda](tpe: Type.Aux[L#TT], lam: L, arg: HA) extends HTree {
  type T = L#TT
}
object Apply {
  def apply[HA <: HTree, L <: Lambda]
    (l: L, arg: HA)
    (implicit
      tTT: ClassTag[L#TT]
    ): Apply[HA, L] =
    Apply[HA, L](Type[L#TT], l, arg)
}

trait Unpack[MA] {
  type M[_]
  type A
  type EV
}
object Unpack {
  type Aux[MA, A0] = Unpack[MA] { type A = A0 }
}

object Interpreter {

  implicit def unpackHTreeAux[M0[_], A0] = new Unpack[M0[A0]] {
    type M[X] = M0[X]
    type A = A0
    type EV = M[A]
  }

  trait Interp[HT <: HTree] {
    def apply(t: HT): t.T
  }
  object Interp {

    def apply[HT <: HTree](implicit int: Interp[HT]) = int

    implicit def constInterp[A]: Interp[Const[A]] =
      new Interp[Const[A]] {
        def apply(t: Const[A]): t.T =
          t.value
      }

    implicit def plusNumericInterp[A: Numeric, LT <: HTree.Aux[A]: Interp, RT <: HTree.Aux[A]: Interp]: Interp[Plus[A, LT, RT]] =
      new Interp[Plus[A, LT, RT]] {
        def apply(t: Plus[A, LT, RT]): t.T =
          implicitly[Numeric[A]].plus(eval(t.lexpr), eval(t.rexpr))
      }

    implicit def lambdaInterp[A, HB <: HTree]: Interp[Lambda.Aux[A, HB]] =
      new Interp[Lambda.Aux[A, HB]] {
        def apply(t: Lambda.Aux[A, HB]): t.T =
          t.f
      }

    implicit def applyInterp[HA <: HTree: Interp, L <: Lambda: Interp]: Interp[Apply[HA, L]] =
      new Interp[Apply[HA, L]] {
        def apply(t: Apply[HA, L]): t.T =
          (eval(t.lam) apply eval(t.arg).asInstanceOf[t.lam.A]).asInstanceOf[t.T]
      }

  }

  def eval[HT <: HTree](t: HT)(implicit int: Interp[HT]): t.T =
    int(t)

  // Tests

  val const0 = Const(Type[Int], 7)
  def const0_test = eval(const0)
  val const1 = Const(2d)
  def const1_test = eval(const1)

  val plus0 = Plus(Type[Int], Const(7), Const(3))
  def plus0_test = eval(plus0)
  val plus1 = Plus(Const(17), plus0)
  def plus1_test = eval(plus1)

  val plus2 = Plus.ap(Const(7), Const(14))
  val plus3 = Plus(Const(0), plus2)

  val lambda0 = Lambda { x: Int => Const(x) }
  def lambda0_test = eval(lambda0)
  val lambda1 = Lambda { x: Int => Plus(Const(x), Const(7)) }
  def lambda1_test = eval(lambda1)

  val apply0 = Apply(lambda0, Const(1))
  def apply0_test = eval(apply0)
}
