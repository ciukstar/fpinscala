package state


trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  type Rand[+A] = RNG => (A,RNG)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r) = f(rng)
      g(a)(r)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if (i + (n-1) - mod > 0) unit(mod) else nonNegativeLessThan(n)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case Nil => (Nil, _)
    case h::t => rng => {
      val (tail,r1) = sequence(t)(rng)
      val (head,r2) = h(r1)
      (head::tail, r2)
    }
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def both[A,B,C](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_,_))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C) =
    flatMap(ra)(a => map(rb)(b => f(a,b)))

  def map22[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    rng => {
      val (a, ra1) = ra(rng)
      val (b, rb1) = rb(ra1)
      (f(a,b), rb1)
    }

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map3[S,A,B](s: S => (A,S))(f: A => B): S => (B,S) =
    st => {
      val (a,r) = s(st)
      (f(a), r)
    }

  def unit[A](a: A): Rand[A] =
    rng => (a,rng)

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(count: Int, tail: List[Int], rng: RNG): (List[Int], RNG) = {
      if (count <= 0) (tail, rng)
      else {
        val (h, r) = rng.nextInt
        go(count - 1, h::tail, r)
      }
    }
    go(count, Nil, rng)
  }

  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(_.nextInt))

  def ints0(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count <= 0) (Nil, rng)
    else {
      val (tail, r1) = ints(count - 1)(rng)
      val (h, r2) = r1.nextInt
      (h::tail, r2)
    }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1,d2,d3),r3)
  }

  def doubleInt: Rand[(Double, Int)] =
    both(double, _.nextInt)

  def doubleInt0(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d), r) = intDouble(rng)
    ((d,i),r)
  }

  def intDouble: Rand[(Int, Double)] =
    both(_.nextInt, double)

  def intDouble0(rng: RNG): ((Int,Double), RNG) = {
    val (i,r) = rng.nextInt
    val (d,r2) = double(r)
    ((i,d),r2)
  }

  def double: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def double0(rng: RNG): (Double, RNG) = {
    val (n,r) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), r)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n,r) = rng.nextInt
    (if (n < 0) -(n+1) else n, r)
  }

  def randomPair(rng: RNG): ((Int,Int), RNG) = {
    val (n1, rng1) = rng.nextInt
    val (n2, rng2) = rng1.nextInt
    ((n1,n2), rng2)
  }
}
