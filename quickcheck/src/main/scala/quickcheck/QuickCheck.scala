package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }


  /**
   * insert any two elements into an empty heap, finding the minimum of the resulting heap should
   * get the smallest of the two elements back.
   */
  property("min2") = forAll { (e1: Int, e2: Int) =>
    val expectedMin = if (e1 < e2) e1 else e2
    findMin(insert(e2, insert(e1, empty))) == expectedMin
  }

  /**
   * Insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
   */
  property("deleteMin") = forAll { (e1: Int) =>
    val h = insert(e1, empty)
    isEmpty(deleteMin(h))
  }

  /**
   * Insert any two elements into separate empty heaps and after that apply meld function for them.
   * Result heap shouldn't be empty
   */
  property("meld1") = forAll { (e1: Int, e2: Int) =>
    val h1 = insert(e1, empty)
    val h2 = insert(e2, empty)
    val h3 = meld(h1, h2)
    !isEmpty(h3)
  }

  /**
   * Given any heap, you should get a sorted sequence of elements when continually
   * finding and deleting minima.
   */
  property("gen1") = forAll { (h1: H, h2: H) =>
    !isEmpty(h1)
  }

  // todo
  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[Int]
    h <- frequency((1, empty), (10, genHeap))
  } yield insert(n, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
