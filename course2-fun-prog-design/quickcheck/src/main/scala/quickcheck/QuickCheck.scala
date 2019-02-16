package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      i <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(i, h)
  )

  def findMinLift(h: H): Option[Int] = {
    if (isEmpty(h)) None
    else Some(findMin(h))
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  // if you find the minimum of a heap, and then insert that same minimum back into the same heap, you should
  // still get the same minimum.
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("insert-two-ordered-elems") = forAll { (i1: Int, i2: Int) =>
    val h1 = empty
    val h2 = insert(i1, h1)
    val finalHeap = insert(i2, h2)
    val finalMin = findMin(finalHeap)

    if (i1 < i2) finalMin == i1
    else finalMin == i2
  }

  property("insert-empty-delete-min") = forAll { i: Int =>
    val h1 = insert(i, empty)
    val finalHeap = deleteMin(h1)
    isEmpty(finalHeap)
  }

  property("repeated-min-yields-ordered-list") = forAll { h: H =>
    def isOrdered(_h: H, priorMin: Option[Int] = None): Boolean = {
      if (isEmpty(_h)) true
      else {
        val newMin = findMin(_h)
        val newHeap = deleteMin(_h)
        if (priorMin.isDefined) {
          if (newMin < priorMin.get) false
          else isOrdered(newHeap, Some(newMin))
        } else {
          isOrdered(newHeap, Some(newMin))
        }
      }
    }
    isOrdered(h)
  }

  property("meld-yields-min-of-one") = forAll { (h1: H, h2: H) =>
    val meldedHeap = meld(h1, h2)

    val min1 = findMinLift(h1)
    val min2 = findMinLift(h2)
    val meldedMin = findMinLift(meldedHeap)

    // TODO this logic feels so clunky, even though it has first-class effects
    if (List(min1, min2, meldedMin).flatten.length == 3) {
      if (min1.get < min2.get) min1.get == meldedMin.get
      else min2.get == meldedMin.get
    } else {
      if (min1.isEmpty && min2.isEmpty) meldedMin.isEmpty
      else if (min1.isEmpty) min2.get == meldedMin.get
      else min1.get == meldedMin.get
    }
  }
}
