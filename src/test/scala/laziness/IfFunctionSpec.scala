package laziness

import org.scalatest.{FlatSpec, OneInstancePerTest, Matchers}

class IfFunctionSpec extends FlatSpec with Matchers with OneInstancePerTest {

  "if2" should "be strict on cond and non-strict in onTrue and onFalse" in {

    val onTrue = { println("one"); 1}
    val onFalse = { println("two"); 2}

    if2(1 < 2, onTrue, onFalse) should be ( 1 )
  }

  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if (cond) onTrue else onFalse
}
