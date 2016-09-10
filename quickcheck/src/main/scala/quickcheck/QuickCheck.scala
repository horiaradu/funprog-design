package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    for {
      elem <- arbitrary[Int]
      heap <- oneOf(const(empty), genHeap)
    } yield insert(elem, heap)
  }
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("should return the same min if reinserting the min in the queue") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("adding two elems in an empty heap, findMin should return the min of the two elems") =
    forAll { (e1: Int, e2: Int) =>
      val queue = insert(e1, insert(e2, empty))
      findMin(queue) == Math.min(e1, e2)
    }

  property("deleteMin on a one elem queue should return an empty queue") =
    forAll { (elem: Int) =>
      val queue = insert(elem, empty)
      isEmpty(deleteMin(queue))
    }

  property("successively calling findMin and deleteMin on a queue should return a sorted list of elems") =
    forAll { (h: H) =>
      val elems = traverseHeap(h)
      elems.sorted == elems
    }

  property("finding the min of a melding of 2 queues should be the min of the mins of the 2 queues") =
    forAll { (h1: H, h2: H) =>
      findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
    }

  property("insert should insert the element in the queue") =
    forAll { (h: H, e: Int) =>
      val elemsBefore = traverseHeap(h)
      val elemsAfter = traverseHeap(insert(e, h))
      elemsAfter == (e :: elemsBefore).sorted
    }

  def traverseHeap(heap: H): List[Int] = {
    def traverseHeapRec(heap: H, acc: List[Int]): List[Int] =
      if (isEmpty(heap)) acc
      else {
        val min = findMin(heap)
        traverseHeapRec(deleteMin(heap), min :: acc)
      }

    traverseHeapRec(heap, Nil).reverse
  }
}
