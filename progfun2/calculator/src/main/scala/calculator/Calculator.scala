package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions mapValues { exprSignal => Signal(eval(exprSignal(), namedExpressions)) }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match {
    case Literal(v) => v
    case Ref(name) => {
      val refExpr = getReferenceExpr(name, references)
      eval(refExpr, references - name) // delete key name from the references map so that cyclic reference will cause error
    }
    case Plus(a, b) => eval(a, references) + eval(b, references)
    case Minus(a, b) => eval(a, references) - eval(b, references)
    case Times(a, b) => eval(a, references) * eval(b, references)
    case Divide(a, b) => eval(a, references) / eval(b, references)
  }

  /**
   * Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
