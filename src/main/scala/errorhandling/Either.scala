package errorhandling

sealed trait Either[+E, +A] {

  def map2[EE >: E, B,C](b: Either[EE,B])(f: (A,B) => C): Either[EE,C] =
    for { aa <- this; bb <- b } yield f(aa,bb)

  def orElse[EE >: E, B >: A](a: => Either[EE,B]): Either[EE,B] = this match {
    case Left(_) => a
    case _ => this
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def map[B](f : A => B): Either[E, B] = this match {
    case Left(x) => Left(x)
    case Right(x) => Right(f(x))
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def traverse[E,A,B](as: List[A])(f: A => Either[E,B]): Either[E,List[B]] = as match {
    case Nil => Right(Nil)
    case h::t => for { hh <- f(h); tt <- traverse(t)(f) } yield hh::tt
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E, List[A]] =
    traverse(es)(x => x)

  def map2[E,A,B,C](a: Either[E,A], b: Either[E,B])(f: (A,B) => C): Either[E,C] =
    a.flatMap(aa => b map (bb => f(aa,bb)))
}
