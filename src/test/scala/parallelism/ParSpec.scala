package parallelism

import java.util.concurrent.{ ExecutorService, Executors, Future }
import org.scalatest.{FlatSpec, OneInstancePerTest, Matchers}

class ParSpec extends FlatSpec with Matchers with OneInstancePerTest {

  "foo" should "bar" in {
    import Par._

    val e0 = Executors.newSingleThreadExecutor()
    val e = Executors.newCachedThreadPool()
    val p1 = map(unit(1))(_  + 1)
    val p2 = unit(2)

    Par.equal(e) (p1, p2) should be ( true )
  }

  "min" should "calculate the min concurrently" in {

    val desc: Par.Par[Int] = min(IndexedSeq(2,3,1,5,4))

    Par.run(Executors.newCachedThreadPool())(desc).get should be ( 1 )
  }

  "max" should "calculate the max concurrently" in {

    val desc: ExecutorService => Future[Int]  = max(IndexedSeq(1,2,5,3,4))

    Par.run(Executors.newCachedThreadPool())(desc).get should be ( 5 )
  }

  "sum" should "calculate the sum concurrently" in {

    val desc: ExecutorService => Future[Int] = sum(IndexedSeq(1,2,3,4,5))

    desc(Executors.newCachedThreadPool()).get should be ( 1+2+3+4+5 )
  }

  def fold[A](as: IndexedSeq[A])(z: A)(f: (A,A) => A): Par.Par[A] =
    if (as.length <= 1)
      Par.unit( as.headOption getOrElse z )
    else {
      val (l,r) = as.splitAt(as.length / 2)
      Par.map2(Par.fork(fold(l)(z)(f)), Par.fork(fold(r)(z)(f)))(f(_,_))
    }

  def min(ints: IndexedSeq[Int]): Par.Par[Int] =
    fold(ints)(Int.MaxValue)(_ min _)

  def sum(ints: IndexedSeq[Int]): Par.Par[Int] =
    fold(ints)(0)(_ + _)

  def max(ints: IndexedSeq[Int]): Par.Par[Int] =
    fold(ints)(Int.MinValue)(_ max _)

  def sumViaFoldLeft(ints: Seq[Int]): Int =
    ints.foldLeft(0)(_ + _)
}
