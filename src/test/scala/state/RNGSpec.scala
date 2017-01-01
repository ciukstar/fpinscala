package state

import org.scalatest.{FlatSpec, OneInstancePerTest, Matchers}

class RNGSpec extends FlatSpec with Matchers with OneInstancePerTest {


  "RNG.ints2(count: Int)(rng: RNG)" should "generate a List of random integers with tailrec optimization" in {
    val seed = 42L
    val (i1,r1) = SimpleRNG(seed).nextInt
    val (i2,r2) = r1.nextInt
    val (i3,r3) = r2.nextInt

    RNG.ints2(3)(SimpleRNG(42))._1 should be ( List(i3,i2,i1) )
  }

  "RNG.ints(count: Int)(rng: RNG)" should "generate a List of random integers" in {
    val seed = 42L
    val (i1,r1) = SimpleRNG(seed).nextInt
    val (i2,r2) = r1.nextInt
    val (i3,r3) = r2.nextInt

    RNG.ints(3)(SimpleRNG(42))._1 should be ( List(i3,i2,i1) )
  }

  "RNG.double" should "generate a Double between 0 and 1, not including 1" in {

    RNG.double(SimpleRNG(42))._1 should ( be >= 0.0 and be < 1.0 )
   }

  "nonNegativeInt(r: RNG)" should "generate an Int between 0 and Int.MaxValue inclusive" in {
    RNG.nonNegativeInt(SimpleRNG(42))._1 should ( be >= 0 )
  }

  "randomPair" should "generate to random integers and return the next state" in {

    val ((n1,n2),rng) = RNG.randomPair(SimpleRNG(42))

    n1 should not be ( n2 )

  }

  "nextInt" should "be testable" in {

    val seed = 42L
    val rng = SimpleRNG(seed)
    val (n1, rng1) = rng.nextInt
    val (n2, rng2) = rng1.nextInt

    n1 should be ( 16159453 )
    rng1 should be ( SimpleRNG(1059025964525L) )
    n2 should be ( -1281479697 )
    rng2 should be ( SimpleRNG( 197491923327988L ) )

  }
}
