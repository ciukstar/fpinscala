package errorhandling

import org.scalatest.{ FlatSpec, OneInstancePerTest, Matchers }


class LiftSpec extends FlatSpec with Matchers with OneInstancePerTest {

  "map2" should "should lift a function" in {

    parseInsuranceRateQuote("23", "66") should be ( Some(99.99) )
  }

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try { age.toInt }
    val optTickets: Option[Int] = Try { numberOfSpeedingTickets.toInt }
    map2(optAge, optTickets)(insuraceRateQuote)
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a flatMap(aa => b map (bb => f(aa,bb)))

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def insuraceRateQuote(age: Int, nuberOfSpeedingTickets: Int): Double = 99.99
}
