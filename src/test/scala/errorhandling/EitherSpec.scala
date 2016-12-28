package errorhandling
import org.scalatest.{FlatSpec, OneInstancePerTest, Matchers}

class EitherSpec extends FlatSpec with Matchers with OneInstancePerTest {

  "safeDiv" should "return either Right or Left if result defined or not" in {

    safeDiv(4, 2) should be ( Right(2))
    safeDiv(4, 0) shouldBe a [ Left[_]]
  }

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x/y)
    catch { case e: Exception => Left(e) }

  "mean" should "return a Right or a Left if mean exists or not respectively" in {
    val xs = IndexedSeq(1.0,2.0,3.0)

    mean(xs) should be ( Right(xs.sum / xs.length) )
    mean(IndexedSeq()) should be ( Left("mean of empty list!") )
  }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty list!")
    else Right(xs.sum / xs.length)

}
