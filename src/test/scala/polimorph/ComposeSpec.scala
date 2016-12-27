package polimorph

import org.scalatest.{FlatSpec, OneInstancePerTest, Matchers}

class ComposeSpec extends FlatSpec with Matchers with OneInstancePerTest {

  "compose" should "compose input function to create the composite" in {

    val g = (a: Int) => 2*a
    val f = (b: Int) => 3*b

    val h = compose(f, g)

    h(3) should be (f(g(3)))
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}
