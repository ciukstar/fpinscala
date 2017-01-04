package parallelism
import java.util.concurrent.{ Callable, ExecutorService, Future, TimeUnit }

object Par {

  type Par[A] = ExecutorService => Future[A]

  def equal[A,B](e: ExecutorService)(pa: Par[A], pb: Par[B]): Boolean =
    pa(e).get == pb(e).get

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] = as map (asyncF(a => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

  def parMap[A,B](as: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = as.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps match {
    case Nil => unit(Nil)
    case h::t => map2(h, fork(sequence(t)))(_ :: _)
  }

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(l: Par[List[Int]]): Par[List[Int]] =
    map(l)(_.sorted)

  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def unit[A](a: A): Par[A] =
    (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(eventIfRunning: Boolean): Boolean = false
    override def get(timeout: Long, unit: TimeUnit): A = get
    override def isCancelled = false
    override def isDone = true
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
  (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }

  def fork[A](a: => Par[A]): Par[A] =
    (es: ExecutorService) => es.submit(new Callable[A] { override def call = a(es).get })

  
}
