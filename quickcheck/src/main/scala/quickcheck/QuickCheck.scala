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
   * Given any two heaps. Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
   */
  property("min3") = forAll { (h1: H, h2: H) =>
    val h1Min = findMin(h1)
    val h2Min = findMin(h2)
    val actualMin = findMin(meld(h1, h2))
    actualMin == h1Min || actualMin == h2Min
  }

  property("meld2") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    heapEqual(meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

  /**
   * Insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
   */
  property("deleteMin") = forAll { (e1: Int) =>
    val h = insert(e1, empty)
    isEmpty(deleteMin(h))
  }

  /**
   * Insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
   */
  property("findMin deleteMin") = forAll { (h1: H) =>
    var mins = List[Int]()
    var heap = h1
    while( !isEmpty(heap) ){
      mins = mins:+ findMin(heap)
      heap = deleteMin(heap)
    }
    sorted(mins)
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



  lazy val genHeap: Gen[H] = {
    val test = for {
      n <- arbitrary[Int]
      h <- frequency((1, empty), (5, genHeap))
    } yield insert(n, h)
    test
  }


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


  def sorted(l:List[Int]) = l.view.zip(l.tail).forall(x => x._1 <= x._2)
}
