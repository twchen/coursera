import common._

// A sealed class may not be directly inherited, except if the inheriting template 
// is defined in the same source file as the inherited class. However, subclasses
// of a sealed class can  be inherited anywhere.
sealed abstract class Tree[A]
case class Leaf[A](a: A) extends Tree[A]
case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

sealed abstract class TreeRes[A] { val res: A }
case class LeafRes[A](override val res: A) extends TreeRes[A]
case class NodeRes[A](l: TreeRes[A],
                      override val res: A,
                      r: TreeRes[A]) extends TreeRes[A]

// convert Tree into TreeRes
def upsweep[A](t: Tree[A], f: (A, A) => A): TreeRes[A] = t match {
  case Leaf(a) => LeafRes(a)
  case Node(l, r) => {
    val (left, right) = parallel(upsweep(l, f), upsweep(r, f))
    NodeRes(left, f(left.res, right.res), right)
  }
}

def downsweep[A](t: TreeRes[A], a0: A, f: (A, A) => A): Tree[A] = t match {
  case LeafRes(a) => Leaf(f(a0, a))
  case NodeRes(l, _, r) => {
    val (tL, tR) = parallel(downsweep(l, a0, f), downsweep(r, f(a0, l.res), f))
    Node(tL, tR)
  }
}

def prepend[A](t: Tree[A], a0: A): Tree[A] = t match {
    case Leaf(a) => Node(Leaf(a0), Leaf(a))
    case Node(l, r) => Node(prepend(l, a0), r)
}

def scanLeft[A](t: Tree[A], a0: A, f: (A, A) => A): Tree[A] = {
    val tRes = upsweep(t, f)
    val t0 = downsweep(tRes, a0, f)
    prepend(t0, a0)
}
