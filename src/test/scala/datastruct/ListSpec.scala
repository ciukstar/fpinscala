package datastruct

import org.scalatest.{ FlatSpec, OneInstancePerTest, Matchers }

class ListSpec extends FlatSpec with Matchers with OneInstancePerTest {

  "zipWith" should "construct a new list by adding corresponding elements" in {

    List.zipWith(List(1,2,3), List(4,5,6))(_ + _) should be ( List(5,7,9) )
    List.zipWith(List(1,2,3,4), List(5,6,7))(_+_) should be ( List(6,8,10) )
  }

  "addPairwise" should "accept two lists and construct a new list by adding corresponding elements" in {

    List.addPairwise(List(1,2,3), List(4,5,6)) should be (List(5,7,9))
  }

  "filterViaFlatMap" should "be defined in terms of flatMap" in {

    List.filterViaFlatMap(List(1,2,3,4))(_ == 2) should be (List(2))
    List.filterViaFlatMap(List(1,2,3,4))(_ > 2) should be (List(3,4))
  }

  "flatMap" should "work like map except that the function given will return a list" in {

    List.flatMap(List(1,2,3))(i => List(i,i)) should be (List(1,1,2,2,3,3))
  }

  "filter" should "remove elements from a list unless they satisfy a given predicate" in {
    List.filter(List(1,2,3,4))(_ > 2) should be ( List(3,4) )
    List.filter(List(1,2,3))(_ < 0) should be ( Nil )
    List.filter(List("a","b","c"))(_ == "b") should be (List("b"))
  }

  "filter0" should "remove elements from a list unless they satisfy a given predicate" in {

    List.filter0(List(1,2,3,4))(_ > 2) should be ( List(3,4) )
    List.filter0(List(1,2,3))(_ < 0) should be ( Nil )
    List.filter0(List("a","b","c"))(_ == "b") should be (List("b"))
  }

  "map" should "modify each element in a list while maintaining the structure of the list" in {

    List.map(List(1,2,3))(_+1) should be (List(2,3,4))
  }

  "doubleToString" should "turn each value in a List[Double] into a String" in {

    List.doubleToString(List(1.1,2.2,3.3)) should be ( List("1.1","2.2","3.3") )
  }

  "addOne" should "transform a list of integers by adding 1 to each element" in {

    List.addOne(List(1,2,3)) should be (List(2,3,4))
  }

  "concat" should "flatten the List[List[A]] to a List[A]" in {

    List.concat(List(List(1,2,3), List(4,5))) should be (List(1,2,3,4,5))
  }

  "appendViaFoldRight" should "concatenate two Lists" in {

    List.appendViaFoldRight(List(1,2,3), List(4,5)) should be (List(1,2,3,4,5))
    List.appendViaFoldRight(List(1,2,3), Nil) should be (List(1,2,3))
    List.appendViaFoldRight(Nil, List(1,2,3)) should be (List(1,2,3))
    List.appendViaFoldRight(Nil, Nil) should be (Nil)
    
  }

  "foldRightViaFoldLeft" should "combine elemenets using foldLeft" in {

    List.foldRightViaFoldLeft(List(1,2,3), 0)(_+_) should be (1+2+3)
    List.foldRightViaFoldLeft(Nil: List[Int], 0)(_+_) should be ( 0 )
  }

  "reverse" should "reverse the List content" in {

    List.reverse(List(1,2,3,4)) should be ( List(4,3,2,1) )
    List.reverse(List(1)) should be ( List(1) )
    List.reverse(Nil) should be ( Nil )
  }

  "lengthLeft" should "calculate List length using leftFold" in {

    List.lengthLeft(List(1,2,3)) should be (3)
    List.lengthLeft(List()) should be (0)
  }

  "foldLeft" should "combine elements of the List using the operation and unit element" in {

    List.foldLeft(List(1,2,3), 0)(_+_) should be (1 + 2 + 3)
    List.foldLeft(List("a", "b", "c"), "")(_+_) should be ("a"+"b"+"c")
    List.foldLeft(Nil: List[Int], 0)(_+_) should be (0)
  }

  "length" should "calculate the length of a List" in {

    List.length(List(1,2,3,4)) should be (4)
    List.length(List(1,2,3)) should be (3)
    List.length(List("a", "b")) should be (2)
    List.length(List()) should be (0)
  }

  "foldRight" should "construct a List when using Nil as unit element and Cons as the operation" in {

    List.foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_)) should be (List(1,2,3))
  }

  "foldRight" should "combine the elements of the list using provided operation and its unit element" in {

    List.foldRight(List(1,2,3), 0)((a,b) => a + b) should be (1+2+3)
    List.foldRight(List(1.0,2.0,3.3), 1.0)(_ * _) should be (1.0 * 2.0 * 3.3)
  }

  "sum and product" should "calculate the sum and the product" in {

    List.sum(List(1,2,3,4)) should be (1 + 2 + 3 + 4)
    List.product(List(1.0, 2.0, 3.0, 4.0)) should be ( 1.0 * 2.0 * 3.0 * 4.0 )
  }

  "init" should "extract all elements but last" in {

    List.init(List(1,2,3,4)) should be ( List(1,2,3) )
    List.init(List(1,2)) should be ( List(1) )
    List.init(List(1)) should be ( Nil )
    List.init(Nil) should be ( Nil )
  }

  "append" should "append a List to another" in {

    List.append(List(1,2), List(3,4)) should be (List(1,2,3,4))
    List.append(Nil, List(3,4)) should be (List(3,4))
    List.append(List(1,2), Nil) should be (List(1,2))
  }

  "dropWhile" should "remove all elements while true" in {

    val l = List(1,2,3,4)

    List.dropWhile(l)(_ < 3) should be (List(3,4))
    List.dropWhile(l)(_ < 4) should be (List(4))
    List.dropWhile(l)(_ < 5) should be (Nil)
  }

  "drop" should "remove a given number of elements from list head" in {

    List.drop(List(1,2,3,4), 2) should be (List(3,4))
    List.drop(List(1,2,3,4), 3) should be (List(4))
    List.drop(List(1,2,3,4), 4) should be (Nil)
    List.drop(List(1,2,3,4), 5) should be (Nil)
    List.drop(List(1,2,3,4), 0) should be (List(1,2,3,4))
    List.drop(List(1,2,3,4), -1) should be (List(1,2,3,4))
  }

  "setHead" should "replace forst element of the list" in {

    List.setHead(List(1,2,3), 0) should be (List(0,2,3))
  }

  "List.tail" should "return a new List without first element" in {

    List.tail(List(1,2,3)) should be (List(2,3))
    List.tail(Nil) should be (Nil)
  }
}
