package polimorph

import org.scalatest.{ FlatSpec, OneInstancePerTest, Matchers }


class PartialAppliedSpec extends FlatSpec with Matchers with OneInstancePerTest {

  "foo" should "bar" in {

    val f = (a: Int, b: Int, c: Int) => a + b + c
    val a = 1
    val b = 2

    val p = partial2(a, b, f)

    p(0) should be (3)
    p(1) should be (4)
  }

  "partial1" should "return a partial function on second argument" in {

    val f = (a: Int, b: Int) => a + b
    val one = 1

    val g = partial1(one, f)

    g(0) should be (1)
    g(1) should be (2)

  }

  def partial2[A,B,C,D](a: A, b: B, f: (A,B,C) => D): C => D =
    c => f(a,b,c)

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a,b)
}
