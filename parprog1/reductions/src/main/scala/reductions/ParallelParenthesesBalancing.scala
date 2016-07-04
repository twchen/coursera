package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balanceRec(idx: Int, left: Int): Boolean = {
      if(idx >= chars.size) left == 0
      else if(left < 0) false
      else if(chars(idx) == '(') balanceRec(idx + 1, left + 1)
      else if(chars(idx) == ')') balanceRec(idx + 1, left - 1)
      else balanceRec(idx + 1, left)
    }
    balanceRec(0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    // arg1, arg2 are not used
    // return a tuple (left, right) which count the imbalanced parentheses
    // e.g. given the input segment "))()(", after canceling the balanced parentheses,
    // the left parentheses are "))(", so (1, 2) is retured.
    // more examples:
    // "abc)()()((" -> ")((" -> (2, 1)
    // "((:))):(((" -> ")(((" -> (3, 1)
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      var i = idx
      var (left, right) = (0, 0)
      while(i < until){
        if(chars(i) == '(') left += 1
        else if(chars(i) == ')'){
            if(left > 0) left -= 1
            else right += 1
        }
        i += 1
      }
      (left, right)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if(until - from <= threshold) traverse(from, until, 0, 0)
      else{
        val mid = from + (until - from) / 2
        val ((l1, r1), (l2, r2)) = parallel(reduce(from, mid), reduce(mid, until))
        if(l1 > r2) (l1 - r2 + l2, r1)
        else (l2, r2 - l1 + r1)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
