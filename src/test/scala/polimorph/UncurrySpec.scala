package polimorph

import org.scalatest.{FlatSpec, OneInstancePerTest, Matchers}

class UncurrySpec extends FlatSpec with Matchers with OneInstancePerTest {

  "uncurry" should "transform a function A => B => C to (A,B) => C" in {

    val f: Int => Int => Int  = (a: Int) => ((b: Int) => a + b)

    val g = uncurry(f)

    g(3,2) should be (5)
  }

  def uncurry[A,B,C](f: A => (B => C)): (A,B) => C =
    (a: A, b: B) => f(a)(b)
}
