package constraints

import cafesat.api.API._

object Circuits {

  type Model = Map[Formula, Boolean]

  /**
   * This function solves implements the circuit described
   * in the [course](http://lara.epfl.ch/~kuncak/talks/2014week10.pdf),
   * page 23.
   */
  def solveExample() : Unit = {
    val p: Formula = boolVar("p")
    val q: Formula = boolVar("q")
    val r: Formula = boolVar("r")

    val p1: Formula = boolVar("p'")
    val q1: Formula = boolVar("q'")

    val c1: Formula = boolVar("c1")
    val c2: Formula = boolVar("c2")

    
    
    val circuit = (
      //p.ensuring(true) &&
      //(p iff true)&&
      (p1 iff !p) &&
      (q1 iff !q) &&
      (c1 iff (p1 || q)) &&
      (c2 iff (q1 || p)) &&
      (r iff (c1 && c2)) &&
      (r iff !p)
    )

    solveForSatisfiability(circuit) match {
      case None => println("UNSAT! Formula is unsatisfiable and has no solutions!")
      case Some(model) => {
        println("SAT: formula is satisfiable! One solution is")
        println("p = " + model(p))
        println("q = " + model(q))
        println("r = " + model(r))
      }
    }
  }

  def main(args: Array[String]) {
    println("Hello Circuits")
    solveExample()
  }
}
