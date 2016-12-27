package polimorph

import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class CurryingSpec extends FlatSpec with Matchers with OneInstancePerTest {

  "curry" should "transform f: (A,B)=>C into A => (B => C)" in {
    val f = (a: Int, b: Int) => a + b

    val c = curry(f)

    c(0)(1) should be (1)
    c(1)(2) should be (3)
  }

  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a,b)

  def partial[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a,b)
}
