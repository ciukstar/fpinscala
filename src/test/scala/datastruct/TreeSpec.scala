package datastruct

import org.scalatest.{FlatSpec, OneInstancePerTest, Matchers}

class TreeSpec extends FlatSpec with Matchers with OneInstancePerTest {

  "map" should "modify each element in the tree with a given function" in {

    Tree.map(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))))(_ + 1) should be (
      Branch(Branch(Leaf(1+1), Leaf(2+1)), Branch(Leaf(3+1), Leaf(4+1)))
    )

    Tree.map2(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))))(_ + 1) should be (
      Branch(Branch(Leaf(1+1), Leaf(2+1)), Branch(Leaf(3+1), Leaf(4+1)))
    )
  }

  "depth" should "return the maximum path length from the root to any leaf" in {

    Tree.depth(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4))) should be ( 3 )
    Tree.depth(Branch(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))), Leaf(5))) should be ( 4 )

    Tree.depth2(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4))) should be ( 3 )
    Tree.depth2(Branch(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))), Leaf(5))) should be ( 4 )
  }

  "maximum" should "return the maximum element in the Tree[Int]" in {

    Tree.maximum(Branch(Branch(Leaf(1), Leaf(3)), Branch(Leaf(5), Leaf(4)))) should be ( 5 )
    Tree.maximum(Branch(Leaf(4), Leaf(3))) should be (4)

    Tree.maximum2(Branch(Branch(Leaf(1), Leaf(3)), Branch(Leaf(5), Leaf(4)))) should be ( 5 )
    Tree.maximum2(Branch(Leaf(4), Leaf(3))) should be (4)
  }

  "size" should "count the number of nodes" in {

    Tree.size(Branch(Leaf(1),Leaf(2))) should be ( 3 )
    Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) should be ( 5 )

    Tree.size2(Branch(Leaf(1),Leaf(2))) should be ( 3 )
    Tree.size2(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) should be ( 5 )
  }
}
