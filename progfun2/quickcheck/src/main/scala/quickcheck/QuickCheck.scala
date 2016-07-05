package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  private def toList(h: H): List[A] = {
    if (isEmpty(h)) Nil
    else findMin(h) :: toList(deleteMin(h))
  }

  private def isSorted(xs: List[A]): Boolean = xs match {
    case x :: y :: rest => ord.lteq(x, y) && isSorted(rest)
    case _ => true
  }

  // only this property is enough to identify all buggy implementations
  // because it uses all functions of heap
  // insert: while generating arbitrary heaps
  // findMin, deleteMin, isEmpty: in toList
  // meld
  property("meld & sorted") = forAll { (h1: H, h2: H) =>
    val xs = toList(meld(h1, h2))
    val ys = (toList(h1) ++ toList(h2)).sorted
    xs == ys
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == math.min(a, b)
  }

  property("empty") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("sorted") = forAll { h: H =>
    isSorted(toList(h))
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    val x = findMin(h1)
    val y = findMin(h2)
    val z = findMin(meld(h1, h2))
    math.min(x, y) == z
  }
}
