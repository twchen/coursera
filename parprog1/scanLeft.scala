import common._

sealed abstract class TreeResA[A] { val res: A }
// each Leaf keeps track of the array segment range (from, to) from which res is computed.
case class Leaf[A](from: Int, to: Int,
    override val res: A) extends TreeResA[A]
case class Node[A](l: TreeResA[A],
    override val res: A,
    r: TreeResA[A]) extends TreeResA[A]

def reduceSeg[A](in: Array[A], from: Int, to: Int, a0: A, f: (A, A) => A): A = {
    var a = a0
    var i = from
    while(i < to){
        a = f(a, in(i))
        i += 1
    }
    a
}

def upsweep[A](in: Array[A], from: Int, to: Int, f: (A, A) => A): TreeResA[A] = {
    if(to - from < threshold)
        Leaf(from, to, reduceSeg(in, from + 1, to, in(from), f))
    else{
        val mid = from + (to - from) / 2
        val (tL, tR) = parallel(upsweep(in, from, mid, f), upsweep(in, mid, to, f))
        Node(tL, f(tL.res, tR.res), tR)
    }
}

def downsweep[A](in: Array[A], a0: A, f: (A, A) => A, t: TreeResA[A], out: Array[A]): Unit = t match {
    case Leaf(from, to, res) => scanLeftSeg(in, from, to, a0, f, out)
    case Node(l, _, r) => {
        parallel(downsweep(in, a0, f, l, out), downsweep(in, f(a0, l.res), f, r, out))
    }
}

def scanLeft(in: Array[A], a0: A, f: (A, A) => A, out: Array[A]) = {
    val t = upsweep(in, 0, in.length, f)
    downsweep(in, a0, f, t, out)
    out(0) = a0
}
