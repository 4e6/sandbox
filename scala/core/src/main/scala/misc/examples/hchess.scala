package misc.examples

import shapeless._, nat._

sealed trait Bool {
  type _B <: Bool
}
object Bool {
  type Aux[B] = Bool { type _B = B }
  val True = new True
  val False = new False
}
class True extends Bool { type B = True }
class False extends Bool { type B = False }

/* OR */

trait OR[A, B] extends Bool

trait ORLowPriorityImplicits {
  implicit def orB[A <: Bool, B <: Bool](implicit ev: B <:< True) =
    new True with OR[A, B]
}

object OR extends ORLowPriorityImplicits {
  type Aux[A, B, R] = OR[A, B] { type _B = R }

  implicit def orA[A <: Bool, B <: Bool](implicit ev: A <:< True) =
    new True with OR[A, B]

  def or[A <: Bool, B <: Bool](a: A, b: B)(implicit ev: A OR B): ev.type = ev
}

/* AND */

trait AND[A, B] extends Bool

object AND {
  type Aux[A, B, R <: Bool] = AND[A, B] { type _B = R}

  implicit def andAB[A <: Bool, B <: Bool]
  (implicit
    evA: A <:< True,
    evB: B <:< True
  ) = new True with AND[A, B]

  def and[A <: Bool, B <: Bool](a: A, b: B)(implicit ev: A AND B): ev.type = ev
}

/* EQ */

trait EQ[A, B] extends Bool

object EQ {
  implicit def eqBool[A <: Bool, B <: Bool](implicit ev1: A <:< True, ev2: B <:< True) =
    new True with EQ[A, B]

  def eqb[A, B](a: A, b: B)(implicit ev: A EQ B): ev.type = ev
}

trait EQN[A <: Nat, B <: Nat] extends Bool

object EQN {
  implicit def eqNat[A <: Nat, B <: Nat](implicit evq: A =:= B) =
    new True with EQN[A, B]

  def eqn[A <: Nat, B <: Nat](a: A, b: B)(implicit ev: A EQN B): ev.type = ev
}

object BoolTests {
  import Bool._, OR.or, AND.and, EQ.eqb, EQN.eqn, nat._

  import shapeless.test.illTyped

  //OR
  implicitly[True OR False]
  implicitly[False OR True]
  implicitly[True OR True]
  illTyped { """implicitly[False OR False]""" }
  or(True, False)
  or(False, True)
  illTyped { """or(False, False)""" }
  val or1 = or(or(True, False), True)
  val or2 = or(or(True, False), False)
  val or3 = or(or(True, False), or(False, True))
  val or4 = or(or(or(True, False), or(False, True)), or(True, True))
  val or5 = or(or4, or3)

  // AND
  implicitly[True AND True]
  illTyped { """implicitly[True AND False]""" }
  illTyped { """implicitly[False AND True]""" }
  illTyped { """implicitly[False AND False]""" }
  val a = and(True, True)
  illTyped { """and(True, False) """}
  illTyped { """and(False, True) """}
  val and1 = and(a, True)
  val and2 = and(and(a, True), and(True, True))
  illTyped { """and(and(False, True), and(True, True))""" }

  // Mixed
  val m1 = and(or(False, True), True)
  val m2 = and(or(True, False), or(False, True))
  val m3 = or(m1, m2)

  // EQ
  eqb(True, True)
  eqb(and1, and2)
  eqb(m2, m3)
  illTyped { """eqb(and1, False)""" }

  // EQN
  trait Check[N <: Nat]
  def check(expected: Nat)(actually : => Check[expected.N]) {}

  def n(x: Nat): x.type = x
  implicitly[_1 EQN _1]
  illTyped { """implicitly[_1 EQ _2]""" }
  val n1 = n(1)
  val n2 = n(2)
  eqn(n1, n1)
  eqn(n2, n2)
  illTyped { """eqn(n1, n2)""" }

  // Mixed
  or(eqb(True, True), False)
  or(eqb(m2, m3), m1)
  or(eqn(n1, n1), m1)
  //or(eqn(n1, n1), eqn(n2, n1))
}

object HQueens {

  sealed trait Piece {
    type X <: Nat
    type Y <: Nat
  }
  object Piece {
    type Aux[X0, Y0] = Piece { type X = X0; type Y = Y0 }
  }

  trait Queen[X0 <: Nat, Y0 <: Nat] extends Piece {
    type X = X0
    type Y = Y0
  }
  object Queen {
    def apply[X <: Nat, Y <: Nat] = new Queen[X, Y] {}
  }

  trait InCheck[P1 <: Piece, P2 <: Piece]
  object InCheck {
    type Aux[
      X0 <: Nat, Y0 <: Nat, P0 <: Piece.Aux[X0, Y0],
      X1 <: Nat, Y1 <: Nat, P1 <: Piece.Aux[X1, Y1]] =
      InCheck[P0, P1]

    implicit def queenInCheck[X0 <: Nat, Y0 <: Nat, X1 <: Nat, Y1 <: Nat]
      (q1: Queen[X0, Y0], q2: Queen[X1, Y1])
      (implicit
        orEv: OR[X0 EQN X1, Y0 EQN Y1]
      ) = new InCheck[Queen[X0, Y0], Queen[X1, Y1]] {}

  }
}
