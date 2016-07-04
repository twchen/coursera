class Poly(terms0: Map[Int, Double]) {
    val terms = terms0 withDefaultValue 0.0
    // the star * sign means the function takes arbitary number of arguments. The type is Seq
    def this(bindings: (Int, Double)*) = this(bindings.toMap)

    def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
    def adjust(term: (Int, Double)) = {
        val (exp, coeff) = term
        exp -> (coeff + terms(exp))
    }
    
    def add (other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))
    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
        val (exp, coeff) = term
        terms + (exp -> (coeff + terms(exp)))
    }
    override def toString = terms.toList.sorted.reverse map { case (p, c) => c + "x^" + p } mkString " + "
}

val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val p2 = new Poly(0 -> 3.0, 3 -> 7.0)
p1 + p2
