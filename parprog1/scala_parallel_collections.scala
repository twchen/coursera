import scala.collection._
import org.scalameter._

object Conversion {
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 10,
    Key.exec.maxWarmupRuns -> 20,
    Key.exec.benchRuns -> 20,
    Key.verbose -> true) withWarmer (new Warmer.Default)

  val memConfig = config(
    Key.exec.minWarmupRuns -> 0,
    Key.exec.maxWarmupRuns -> 0,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true) withWarmer (Warmer.Zero)

  val vector = Vector.fill(1e7)("")
  val list = vector.toList

  def main(args: Array[String]) {
    val listtime = standardConfig measure {
        list.par
    }
    println(s"list conversion time: $listtime ms")

    val vectortime = standardConfig measure {
        vector.par
    }

    println(s"vector conversion time: $vectortime ms")
    println(s"difference: ${listtime / vectortime}")
  }
}
