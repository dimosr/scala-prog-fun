package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      n <- arbitrary[Int]
      heap <- insert(n, empty)
      next <- oneOf(const(empty), genHeap)
    } yield meld(heap, next)
  )
  lazy val nonEmptyGenHeap: Gen[H] = for {
    n <- arbitrary[Int]
    heap <- insert(n, empty)
    next <- oneOf(const(empty), genHeap)
  } yield meld(heap, next)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("inserting 2 elements, puts smallest on top") = forAll { (n1: Int, n2: Int) =>
    val emptyHeap = empty
    val min = Math.min(n1, n2)
    val newHeap = insert(n2, insert(n1, empty))
    findMin(newHeap) == min
  }

  property("inserting and removing element from empty heap leads to an empty heap") = forAll { (n: Int) =>
    val heapAfterAddition = insert(n, empty)
    val heapAfterRemoval = deleteMin(heapAfterAddition)
    isEmpty(heapAfterRemoval)
  }

  property("popped elements are in ascending order") = forAll(genHeap) { (h: H) =>
    def checkOrderAcc(h: H, acc: List[A]): Boolean = {
      if(isEmpty(h))
        true
      else {
        val current = findMin(h)
        acc match {
          case List() => checkOrderAcc(deleteMin(h), List(current))
          case previous::rest => (current >= previous ) && checkOrderAcc(deleteMin(h), current::acc)
        }
      }
    }

    checkOrderAcc(h, List())
  }

  property("minimum of melded heaps is minimum of the minimums of the 2 heaps") = forAll(nonEmptyGenHeap, nonEmptyGenHeap) { (h1: H, h2: H) =>
    val meldedHeap = meld(h1, h2)
    findMin(meldedHeap) == Math.min(findMin(h1), findMin(h2))
  }

  property("oddly melded lists are equal") = forAll(nonEmptyGenHeap, nonEmptyGenHeap) { (h1: H, h2: H) =>
    def isHeapEqual(h1: H, h2: H): Boolean = {
      def isEqualIter( status: Boolean, h1: H, h2: H): Boolean = {
        if (isEmpty(h1))
          if (isEmpty(h2))
            true
          else
            false
        else
          status && isEqualIter(findMin(h1) == findMin(h2), deleteMin(h1), deleteMin(h2))
      }

      isEqualIter( true, h1, h2)
    }

    isHeapEqual(meld(deleteMin(h1), insert(findMin(h1), h2)), meld(h1, h2))
  }

}
