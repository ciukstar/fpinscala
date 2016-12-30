package errorhandling
import org.scalatest.{FlatSpec, OneInstancePerTest, Matchers}

class EitherSpec extends FlatSpec with Matchers with OneInstancePerTest {

  "traverse" should "apply given transformation to each element and return Right for all transformations or a Left" in {

    Either.traverse(List("1","2","3"))(a => Try { a.toInt }) should be ( Right(List(1,2,3)) )
    Either.traverse(List("1","b","3"))(a => Try { a.toInt }) shouldBe a [ Left[_] ]
  }

  "sequence" should "transform a List of Either to an Either of all Right or a Left" in {

    Either.sequence(List(Right(1), Right(2), Right(3))) should be ( Right(List(1,2,3)) )
    Either.sequence(List(Right(1), Left("Error"), Right(3))) should be ( Left("Error") )
  }

  "orElse" should "return the result of passed expression if its value evaluates to a Left" in {

    Try { "1".toInt } orElse Try { "2".toInt } should be ( Right(1) )
    Try { "a".toInt } orElse Try { "2".toInt } should be ( Right(2) )
    Try { "1".toInt } orElse Try { "a".toInt } should be ( Right(1) )
    Try { "a".toInt } orElse Try { "b".toInt } shouldBe a [ Left[_] ]
  }

  "flatMap" should "apply a transformation to the right value that itself could fail" in {

    Right("1").flatMap( a => Try { a.toInt } ) should be ( Right(1) )
    Right("b").flatMap(a => Try { a.toInt }) shouldBe a [Left[_]]
    (Left(new Exception()): Either[Exception, String]).flatMap( a => Try { a.toInt } ) shouldBe a [Left[_]]
  }

  "map" should "apply given transformation to the Right value otherwise the result shiuld be a Left" in {

    Right(1).map(_ + 1) should be ( Right(2) )
    (Left(new Exception()): Either[Exception, Int]).map(_ + 1) shouldBe a [Left[_]]
  }

  "Try" should "return a Right if alright and a Left otherwise" in {

    Try { "1".toInt } should be ( Right(1) )
    Try { "a".toInt } shouldBe a [Left[_]]
  }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

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
