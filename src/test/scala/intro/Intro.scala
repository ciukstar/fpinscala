package intro

import org.scalatest.{FlatSpec, OneInstancePerTest, Matchers}

class CafeSpec extends FlatSpec with Matchers with OneInstancePerTest {

  "foo" should "bar" in {

  }

}

class Cafe {
  def byCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purchases: List[(Coffee, Charge)] = List.fill(n)(byCoffee(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce((c1,c2) => c1.combine(c2)))
  }

  def byCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = new Coffee(99.89)
    (cup, Charge(cc, cup.price))
  }
}

trait Payments {
  def charge(cc: CreditCard, amount: Double): Unit
}

case class Charge(val cc: CreditCard, val amount: Double) {
  def combine(other: Charge): Charge = {
    if (cc == other.cc)
      Charge(cc, amount + other.amount)
    else
      throw new Exception("Can't combine charges of diffrent cards")
  }
}
case class CreditCard(val number: Int)
case class Coffee(val price: Double)
