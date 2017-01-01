package laziness

sealed trait Stream[+A] {

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((h,t) => f(h) append t)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => Stream.cons(h, t))

  def appendViaMatch[B >: A](s: => Stream[B]): Stream[B] = this match {
    case Empty => s
    case Cons(h, t) => Stream.cons(h(), t().append(s))
  }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h,t) => if (p(h)) Stream.cons(h,t) else t)

  def map[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(h,t) => Some((f(h()), t()))
      case _ => None
    }

  def map0[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((h,t) => Stream.cons(f(h), t))

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h,t) else Stream.empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Empty => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
  }

  def exists(p: A => Boolean): Boolean =
    foldRight (false)((a,b) => p(a) || b)

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t() takeWhile p)
    case _ => Stream.empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def take2(n: Int): Stream[A] =
    Stream.unfold((this,n)) {

    }

  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 1 => Stream.cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => Stream.cons(h(), Empty)
    case _ => Stream.empty
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h,t) => h()::t().toList
  }

  def headOptionViaFoldRight(): Option[A] =
    foldRight(None: Option[A])((h,t) => Some(h))

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h,t) => Some(h())
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a,s)) => cons(a, unfold(s)(f))
  }

  val fibs =
    unfold((0,1)) { case (f0, f1) => Some((f0,(f1, f0 + f1))) }

  val fibs0: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] =
      cons(a, go(b, a + b))

    go(0,1)
  }

  def from(n: Int): Stream[Int] =
    unfold(n)(n => Some((n,n+1)))

  def from0(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def constant[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a,a)))

  def constant0[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = cons(a, tail)
    tail
  }

  def foldRight[A,B](s: Stream[A], z: => B)(f: (A, => B) => B): B = s match {
    case Empty => z
    case Cons(h, t) => f(h(), foldRight(t(), z)(f))
  }

  def exists[A](s: Stream[A])(p: A => Boolean): Boolean = s match {
    case Cons(h, t) => p(h()) || exists(t())(p)
    case _ => false
  }

  def takeWhile[A](s: Stream[A])(p: A => Boolean): Stream[A] = s match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), takeWhile(t())(p))
    case _ => Stream.empty
  }

  def drop[A](s: Stream[A], n: Int): Stream[A] = s match {
    case Cons(h, t) if n > 0 => drop(t(), n-1)
    case _ => s
  }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
