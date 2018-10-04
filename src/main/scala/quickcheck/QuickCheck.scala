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
        k <- arbitrary[A]
        h <- oneOf(const(empty), genHeap)
      } yield insert(k, h)
    )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

    property("min1") = forAll { a: Int =>
      val h = insert(a, empty)
      findMin(h) == a
    }

    property("min2") = forAll { (a1: A, a2: A) =>
      val smallest = if (a1 < a2) a1 else a2
      val b = insert(a2, insert(a1, empty))
      findMin(b) == smallest
    }

    property("emp1") = forAll { a: A =>
      isEmpty(deleteMin(insert(a, empty)))
    }

    property("seq1") = forAll { h: H =>
      def loop(h1: H, acc: Boolean): Boolean = {
        if (isEmpty(h1) || isEmpty(deleteMin(h1)) || !acc) acc
        else loop(deleteMin(h1), acc && (findMin(h1) <= findMin(deleteMin(h1))))
      }

      loop(h, true)
    }

  property("mel1") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) || isEmpty(h2)) true
    else {
      val h3 = meld(h1, h2)
      findMin(h3) == findMin(h1) || findMin(h3) == findMin(h2)
    }

  }

  property("com2") = forAll { (a: A, h: H) =>
    def loop(h1: H, acc: Boolean): Boolean = {
      if (isEmpty(h1) || acc) acc
      else loop(deleteMin(h1), acc || findMin(h1) == a)
    }

    loop(insert(a, h), false)
  }

}
