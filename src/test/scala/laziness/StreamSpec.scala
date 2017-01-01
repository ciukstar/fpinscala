package laziness

import org.scalatest.{FlatSpec, OneInstancePerTest, Matchers}

class StreamSpec extends FlatSpec with Matchers with OneInstancePerTest {

  "Stream.unfold(Empty: Int)(s => Some((1, cons(1, Empty))))" should "generate an infinite stream of ones" in {
    import Stream._

    val ones = Stream.unfold(Stream.empty[Int])(s => Some((1, cons(1, s))))

    ones.take(5).toList should be ( List(1,1,1,1,1) )
  }

  "Stream.fibs" should "generate the infinite stream of Fibonacci number" in {
    ( Stream.fibs take 7 ).toList should be ( List(0,1,1,2,3,5,8) )
  }

  "Stream.from" should "generate an infinite stream of integers starting from n, then n + 1, n + 2, and so on" in {

    val s = Stream.from(2)

    s shouldBe a [ Stream[_] ]
    s.take(5).toList should be ( List(2,3,4,5,6) )
  }

  "Stream.constant" should "create an infinite Stream of given value" in {

    val s = Stream.constant(1)

    s shouldBe a [Stream[_]]
    s.take(3).toList shouldBe ( List(1,1,1) )
    s.map(_ + 1).exists(_ % 2 == 0) should be (true)
  }

  "applying tarnsformations on infinite stream" should "give correct result" in {
    import Stream._

    lazy val ones: Stream[Int] = Stream.cons(1, ones)

    ones.take(5).toList should be ( List(1,1,1,1,1) )
    ones.map(_ + 1).exists(_ % 2 == 0) should be ( true )
    ones.takeWhile(_ == 1) should not be ( Empty )
    ones.forAll(_ != 1) should be ( false )
  }

  "find(p: A => Boolean)" should "find the first element of the Strem that matches the predicate" in {
    import Stream._

    cons(1, cons(2, cons(3, Empty))).find(_ > 1) should contain ( 2 )
    cons(1, cons(2, Empty)).find(_ > 2) should be ( None )
  }

  it should "stop Strem traversal as soon as it finds first element that matches the predicate" in {
    import Stream._

    cons(1, cons(2, cons(fail(), Empty))).find(_ > 1) should contain ( 2 )
  }

  "flatMap" should "transform this Stream using given function" in {
    import Stream._

    cons(1, cons(2, Empty)).flatMap(x => cons(x, cons(x, Empty))).toList should be ( List(1,1,2,2) )
    cons(1, cons(2, Empty)).flatMap(x => Empty) should be ( Empty )
    Empty.flatMap(x => Empty) should be ( Empty )
  }

  it should "be non-strict in its arguments" in {
    import Stream._

    cons(1: Int, cons(fail(), Empty)).flatMap(x => cons(2*x, Empty))
  }

  "append" should "attach given Stream to the end of this Stream" in {
    import Stream._

    cons(1, cons(2, Empty)).append(cons(3, cons(4, Empty))).toList should be ( List(1,2,3,4) )
    Stream.empty.append(cons(3, cons(4, Empty))).toList should be ( List(3,4) )
    cons(1, cons(2, Empty)).append(Empty).toList should be (List(1,2))
    Empty.append(Empty) should be ( Empty )
  }

  it should "be non-strict in its arguments" in {
    import Stream._

    cons(1, cons(fail(), cons(fail(), Empty))).append(cons(fail(), Empty))
  }

  "appendViamatch" should "append given Stream to the end of this" in {
    import Stream._

    val as: Stream[Int] = cons(3,cons(4, Empty))

    cons(1, cons(2, Empty)).appendViaMatch(as).toList should be ( List(1,2,3,4) )
    cons(1, cons(2, Empty)).appendViaMatch(Empty).toList should be ( List(1,2) )
    Stream.empty.appendViaMatch(as).toList should be ( List(3,4) )
    Stream.empty.appendViaMatch(Empty) should be ( Empty )
  }

  it should "not evaluate this Stream and appended Stream" in {
    import Stream._

    cons(fail(), cons(fail(), Empty)).appendViaMatch(cons(fail(), Empty))
  }

  "filter" should "remove elements that do not match given predicate" in {
    import Stream._

    cons(2, cons(4, cons(6, Empty))).filter(_ % 2 == 0).toList should be ( List(2,4,6) )
    cons(2, cons(3, cons(6, Empty))).filter(_ % 2 == 0).toList should be ( List(2,6) )
    Stream.empty[Int].filter(_ % 2 == 0) should be ( Empty )
  }

  "map" should "transform each element of the Stream using passed function" in {
    import Stream._

    cons(1, cons(2, cons(3, Empty))).map(_ + 1).toList should be ( List(2,3,4) )
  }

  "headOptionViaFoldRight" should "extract Some head of the Stream if any" in {
    import Stream._

    cons(1, cons(2, cons(3, Empty))).headOptionViaFoldRight() should be ( Some(1) )
    Stream.empty.headOptionViaFoldRight() should be ( None )
  }

  it should "not evaluate the rest of the stream" in {
    import Stream._

    cons(1, cons(fail(), cons(fail(), Empty))).headOptionViaFoldRight() should be ( Some(1) )
  }

  "takeWhileViaFoldRight(p: A => Boolean)" should "return a Stream of all first elements that matches p" in {
    import Stream._

    cons(1, cons(2, cons(3, Empty))).takeWhileViaFoldRight(_ < 3).toList should be ( List(1,2) )
    cons(1, cons(2, cons(3, Empty))).takeWhileViaFoldRight(_ > 3) should be ( Empty )
    (Stream.empty[Int]).takeWhileViaFoldRight(_ < 3) should be ( Empty )
  }

  it should "terminate traversal at the element that does not match the predicate" in {
    import Stream._

    cons(1,cons(2, cons(fail(), Empty))).takeWhileViaFoldRight(_ < 2).toList should be ( List(1) )
  }

  "forAll" should "check that all elements in a Stream match a given predicate" in {
    import Stream._

    cons(2, cons(4, cons(6, Empty))).forAll(_ % 2 == 0) should be ( true )
  }

  it should "terminate traversal as soon as it encounters a nonmatching value" in {
    import Stream._

    cons(2, cons(3, cons(fail(), Empty))).forAll( _ % 2 == 0 ) should be ( false )
  }

  "foldRight" should "reduce this Stream to a value using a combiner f and a value to start with" in {
    import Stream._

    cons(1, cons(2, cons(3, Stream.empty))).foldRight(0)(_ + _) should be ( 1+2+3 )
    (Stream.empty: Stream[Int]).foldRight(0)(_ + _) should be ( 0 )
  }

  "Stream.foldRight" should "reduce the Stream to a value using a given transformation and a value to begin with" in {
    import Stream._

    foldRight(cons(1, cons(2, cons(3, Stream.empty))), 0)(_ + _) should be ( 1+2+3 )
    foldRight(Stream.empty: Stream[Int], 0)(_ + _) should be ( 0 )
  }

  "exists(p: A => Boolean)" should "check if this Stream contains at least one element that matches p" in {
    import Stream._

    cons(1, cons(2, cons(fail("Three"), Stream.empty))).exists(_ == 2) should be ( true )
    cons(1, cons(fail("Two"), cons(fail("Three"), Stream.empty))).exists(_ < 3) should be ( true )
    cons(1, cons(2, cons(3, Stream.empty))).exists(_ > 3) should be ( false )
  }

  "Stream.exists(s: Stream[A])(p: A => Boolean)" should "check if the stream contains an element that matches p" in {
    import Stream._

    exists(cons(1, cons(2, cons(fail("Three"), Stream.empty))))(_ == 2) should be ( true )
    exists(cons(1, cons(2, cons(3, Stream.empty))))(_ == 0) should be ( false )
  }

  "takeWhile(p: A => Boolean)" should "return all elements of this Stream that match p" in {
    import Stream._

    cons(1, cons(2, cons(3, Empty))).takeWhile(_ < 3).toList should be ( List(1,2) )
    cons(1, cons(2, cons(3, Stream.empty))).takeWhile(_ < 1).toList should be ( Nil )
  }

  "Stream.takeWhile(s: Stream)(p: A => Boolean)" should "return all elements of the stream that match p" in {
    import Stream._

    takeWhile(cons(1, cons(2, cons(3, Empty))))(_ < 3).toList should be ( List(1,2) )
    takeWhile(cons(1, cons(2, cons(3, Empty))))(_ < 1).toList should be ( Nil )
  }

  "drop(n)" should "skip first n elements of a Stream" in {
    import Stream._

    cons(fail("One"), cons(2, cons(3, Empty))).drop(1).toList should be ( List(2,3) )
    cons(fail("One"),cons(fail("Two"), cons(3, Empty))).drop(2).toList should be ( List(3) )
    cons(fail(), cons(fail(), cons(fail(), Empty))).drop(3).toList should be ( Nil )
  }

  "Stream.drop(n: Int)" should "skip first n elements of a Stream. Elements should not be evaluated" in {
    import Stream._

    drop(cons({ fail("One") }, cons(2, cons(3, Empty))), 1).toList should be ( List(2,3) )
    drop(cons({ fail("One") }, cons( { fail("Two") } , cons(3, Empty))), 2).toList should be ( List(3) )
    drop(cons(1, cons(2, cons(3, Empty))), 3).toList should be ( List() )
  }

  "take(n: Int)" should "return the first n elements of a Stream" in {
    import Stream._

    cons(1,cons(2, cons({ fail("Three") }, Empty))).take(2).toList should be ( List(1,2) )
  }

  "toList" should "convert the Stream to a List by forcing the thunk" in {
    Stream(1,2,3).toList should be ( List(1,2,3) )
    Stream().toList should be ( Nil )
  }

  "headOption" should "return the head if any. The tail should not be evaluated" in {
    Stream(1,2,3).headOption should be ( Some( 1 ) )
    Stream().headOption should be ( None )
  }
}
