class Signal[T](expr: => T) {
  protected var myExpr = () => expr
  def apply(): T = {
    println("apply")
    myExpr()
  }
}

object Signal {
  def apply[T](expr: => T) = new Signal(expr)
}

class Var[T](expr: => T) extends Signal[T](expr) {
  def update(newexpr: => T): Unit = {
    println("update: " + newexpr)
    myExpr = () => newexpr
  }
}

object Var {
  def apply[T](expr: => T) = new Var(expr)
}
