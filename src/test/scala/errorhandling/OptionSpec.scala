package errorhandling

import org.scalatest.{FlatSpec, OneInstancePerTest, Matchers}

class OptionSpec extends FlatSpec with Matchers with OneInstancePerTest {

  "traverse" should "transform each element using given function to return Some list of values" in {

    Option.traverse(List("1","2","3"))(s => Option.Try(s.toInt)) should be ( Some(List(1,2,3)) )
    Option.traverse( List("1", "b", "3") )( s => Option.Try { s.toInt } ) should be ( None )
    Option.traverse( Nil: List[String] )( s => Option.Try { s.toInt } ) should be ( Some(Nil) )
  }

  "sequence" should "combine a list of Option into one Option containing of all Some values" in {

    Option.sequence(List(Some(1), Some(2), Some(3))) should be ( Some(List(1,2,3)) )
    Option.sequence(List(Some(1), None, Some(3))) should be ( None )
  }

  "map2" should "combine two Optiona values using a binary function" in {

    map2(Some(1), Some(2))(_ + _) should be ( Some(3) )
    map2(None: Option[Int], Some(2))(_ + _) should be ( None )
    map2(Some(1), None: Option[Int])(_ + _) should be ( None )
  }


  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))


  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  "variance" should "fail with a result of None for an empty sequence" in {

    val xs: Seq[Double] = Seq()

    variance(xs) should be(None)
  }

  "variance" should "calculate the variance of a sequence of some doubles" in {

    val xs = Seq(1.0, 2.0, 3.0, 4.0)
    val x = Some(4.0)
    val m = (xs.sum / xs.length)

    variance(xs) should be(Some(1.25))
  }

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  "filter(f)" should "return Some(x) if f(x) else None" in {

    val x: Option[Int] = Some(3)
    val y: Option[Int] = None

    x.filter(_ == 3) should be(Some(3))
    x.filter(_ == 2) should be(None)
    y.filter(_ == 3) should be(None)
  }

  "orElse" should "return the Some or else given ob" in {

    val x: Option[Int] = Some(3)
    val y: Option[Int] = None

    x.orElse(Some(-1)) should be(Some(3))
    y.orElse(Some(-1)) should be(Some(-1))
  }

  "getOrElse" should "extract wrapped value if Some or else return default" in {

    val x: Option[Int] = Some(3)
    val y: Option[Int] = None

    x.getOrElse(-1) should be(3)
    y.getOrElse(-1) should be(-1)
  }

  "flatMap" should "apply the function on Some value or else return None" in {
    val x: Option[Int] = Some(3)
    val y: Option[Int] = None

    x.flatMap(a => Some(a)) should be(Some(3))
    x.flatMap(a => None) should be(None)
    y.flatMap(a => Some(a)) should be(None)
    y.flatMap(a => None) should be(None)
  }

  "map" should "apply the given function if Option is not None" in {

    val x: Option[Int] = Some(7)
    val y: Option[Int] = None

    x.map(_ + 3) should be(Some(10))
    y.map(_ + 3) should be(None)

  }

  "mean" should "calculate mean value of the elements in the sequence" in {
    mean(Seq(1, 2, 3)) should be(Some(2))
    mean(Seq()) should be(None)
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
}
