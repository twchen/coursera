import scala.collection._

object IntersectionWrong {
  def main(args: Array[String]) {
    def intersection(a: GenSet[Int], b: GenSet[Int]): Set[Int] = {
      val result = mutable.Set[Int]()
      for (x <- a) if (b contains x) result += x
      result
    }
    val seqres = intersection((0 until 1000).toSet, (0 until 1000 by 4).toSet)
    val parres = intersection((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet)
    log(s"Sequential result - ${seqres.size}")
    log(s"Parallel result - ${parres.size}")
  }
}
// If a is a parallel set, the for loop will be executed in parallel, so the result set will be updated concurrently. Since mutable set is not thread-safe, concurrent modifications can corrupt its internal state

// solution 1 - use a concurrent collection
import java.util.concurrent._
val result = new ConcurrentSkipListSet[Int]()

// solution 2 - avoid side-effects
def intersection(a: GenSet[Int], b: GenSet[Int]): Set[Int] = {
  if (a.size < b.size) a.filter(b(_))
  else b.filter(a(_))
}

val graph = mutable.Map[Int, Int]() ++= (0 until 100000).map(i => (i, i + 1))
graph(graph.size - 1) = 0
for ((k, v) <- graph.par) graph(k) = graph(v)
val violation = graph.find({ case (i, v) => v != (i + 2) % graph.size })
println(s"violation: $violation")

// using TrieMap
val graph = concurrent.TrieMap[Int, Int]() ++= (0 until 100000).map(i => (i, i + 1))
graph(graph.size - 1) = 0

// snapshot is very efficient, O(1)
val previous = graph.snapshot()
for ((k, v) <- graph.par) graph(k) = previous(v)
val violation = graph.find({ case (i, v) => v != (i + 2) % graph.size })
println(s"violation: $violation")
