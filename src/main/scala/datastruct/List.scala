package datastruct

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A,B) => C): List[C] = (l1,l2) match {
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(h1,t1),Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }

  def addPairwise(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1,t2))
  }

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h,t) => if (f(h)) Cons(h,t) else t)

  def filter0[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h,t) if f(h) => Cons(h, filter0(t)(f))
    case Cons(_,t) => filter0(t)(f)
  }


  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((h,t) => Cons(f(h), t))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h,t) => Cons(h.toString, t))

  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((h,t) => Cons(h+1,t))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((a,b) => Cons(a,b))

  def foldRightViaFoldLeft2[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b: B) => b)((g,a) => b => g(f(a,b)))(z)

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), z)((b,a) => f(a,b))

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((acc, h) => Cons(h, acc))

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = as match {
    case Nil => z
    case Cons(x,xs) => foldLeft(xs, f(z, x))(f)
  }

  def lengthLeft[A](as: List[A]): Int =
    foldLeft(as, 0)((b,a) => b + 1)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((a,b) => 1 + b)

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
    case Nil => z
    case Cons(x,xs) => f(x, foldRight(xs, z)(f))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t,a2))
  }

  def dropWhile[A](l: List[A])(p: A => Boolean): List[A] = l match {
    case Cons(x,xs) if p(x) => dropWhile(xs)(p)
        case _ => l
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(x,xs) if n <= 0 => Cons(x,xs)
    case Cons(_,xs) => drop(xs, n-1)
  }

  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil => Nil
    case Cons(_,xs) => Cons(a,xs)
  }

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(_,xs) => xs
  }

  def sumLeft(ints: List[Int]): Int =
    foldLeft(ints, 0)(_+_)

  def sum(ints: List[Int]): Int =
    foldRight(ints, 0)((a,b) => a + b)

  def productLeft(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_*_)

  def product(ds: List[Double]): Double =
    foldRight(ds, 1.0)((a,b) => a * b)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
