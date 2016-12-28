package errorhandling

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def orElse2[B >: A](ob: => Option[B]): Option[B] =
    this map(Some(_)) getOrElse ob

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) if f(x) => Some(x)
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h::t => traverse(t)(f) flatMap (tt => f(h) map (hh => hh :: tt) )
  }

  def parseInts(a: List[String]): Option[List[Int]] =
    sequence(a map (i => Try { i.toInt }))

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a flatMap(aa => b map (bb => f(aa,bb)))

  def map3[A,B,C,D](a: Option[A], b: Option[B], c: Option[C])(f: (A,B,C) => D): Option[D] =
    a flatMap(aa => b flatMap (bb => c map (cc => f(aa,bb,cc))))


}
