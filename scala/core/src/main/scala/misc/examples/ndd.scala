package misc.examples

// Naive effect system
sealed trait Effect

case object Nothing extends Effect

case object Effect extends Effect {
  def apply[A](a: ⇒ A): Effect = a match {
    case Nothing ⇒ Nothing
    case _       ⇒ this
  }
}

object NullDrivenDevelopment {

  // Don't dare to call it a Monad, so it's just MO
  implicit class NullMO[A <: AnyRef](a: A) {

    def map[B <: AnyRef](f: A ⇒ B): B =
      if (a ne null) f(a)
      else null.asInstanceOf[B]

    def flatMap[B <: AnyRef](f: A ⇒ B): B =
      map(f)

    def filter(p: A ⇒ Boolean): A =
      if ((a ne null) && p(a)) a
      else null.asInstanceOf[A]

    def withFilter(p: A ⇒ Boolean): A =
      filter(p)

    def foreach[B](f: A ⇒ B): Effect =
      if (a ne null) Effect(f(a))
      else Nothing
  }
}

object NDDExample {
  import NullDrivenDevelopment._

  def none: Pojo = null
  def pojo: Pojo = new Pojo

  // NPE
  def testNPE(pojo: Pojo = none) = pojo.toString

  // null
  def testMap(pojo: Pojo = none) = for {
    p ← pojo
  } yield p.toString

  // null
  def testFlatMap(pojo: Pojo = none) = for {
    p ← new Pojo
    q ← pojo
  } yield q.toString

  // null
  def testFilter() = for {
    p ← pojo
    q ← pojo
    if p.toString == q.toString
  } yield p

  // Vector(m,i,s,c,...)
  def testNotNull() = for {
    p ← pojo
    q ← pojo
    ps = p.toString
    qs = q.toString
    (x, y) ← ps zip qs
    if x == y
  } yield x

  // Effect
  def testEffect() = for {
    p ← new Pojo
  } p.doStuff()

  // Nothing
  def testNothing() = for {
    p ← new Pojo
    n ← none
  } n.doStuff()
}

class Pojo {
  def doStuff(): Unit = println("rocket launched!")
}
