package parallelism

import org.scalatest.{FlatSpec, OneInstancePerTest, Matchers}

class TheProcessSpec extends FlatSpec with Matchers with OneInstancePerTest {

  "foo" should "bar" in {

    sum(IndexedSeq(1, 2, 3, 4, 5)) should be(1 + 2 + 3 + 4 + 5)
  }

  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val sumL = Par.unit(sum(l))
      val sumR = Par.unit(sum(r))
      Par.get(sumL) + Par.get(sumR)
    }
}
