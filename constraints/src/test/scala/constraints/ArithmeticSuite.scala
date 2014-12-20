package constraints

import regolic.asts.fol.Trees._
import cafesat.api.API._

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ArithmeticSuite extends FunSuite {

  import Arithmetic._

  test("binary to int conversion") {
    assert(binary2int(List(true)) === 1)
    assert(binary2int(List(true, false)) === 2)
    assert(binary2int(List(true, true)) === 3)
    assert(binary2int(List(true, false, false)) === 4)
    assert(binary2int(List(true, false, true)) === 5)
    assert(binary2int(List(true, true, false)) === 6)
    assert(binary2int(List(true, true, true)) === 7)
    assert(binary2int(List(true, false, false, false)) === 8)
    assert(binary2int(List(true, false, false, true)) === 9)
  }

  test("int to binary conversion") {
    assert(int2binary(1) === List(true))
    assert(int2binary(2) === List(true, false))
    assert(int2binary(3) === List(true, true))
    assert(int2binary(4) === List(true, false, false))
    assert(int2binary(5) === List(true, false, true))
    assert(int2binary(6) === List(true, true, false))
  }

  test("lessEquals between constant numbers of one bit") {
    val na1: List[Formula] = List(false)
    val na2: List[Formula] = List(false)
    val ra = lessEquals(na1, na2)
    assert(solveForSatisfiability(ra) != None)

    val nb1: List[Formula] = List(false)
    val nb2: List[Formula] = List(true)
    val rb = lessEquals(nb1, nb2)
    assert(solveForSatisfiability(rb) != None)

    val nc1: List[Formula] = List(true)
    val nc2: List[Formula] = List(true)
    val rc = lessEquals(nc1, nc2)
    assert(solveForSatisfiability(rc) != None)

    val nd1: List[Formula] = List(true)
    val nd2: List[Formula] = List(false)
    val rd = lessEquals(nd1, nd2)
    assert(solveForSatisfiability(rd) === None)
  }

  test("less equals between constants is working") {

    val na1: List[Formula] = List(true, false, true)
    val na2: List[Formula] = List(true, true, true)
    val ra = lessEquals(na1, na2)
    assert(!solveForSatisfiability(ra).isEmpty)

    val nb1: List[Formula] = List(true, false, true)
    val nb2: List[Formula] = List(false, true, true)
    val rb = lessEquals(nb1, nb2)
    assert(solveForSatisfiability(rb) === None)

    val nc1: List[Formula] = List(true, false, true)
    val nc2: List[Formula] = List(true, false, false)
    val rc1 = lessEquals(nc1, nc2)
    val rc2 = lessEqualsConst(nc1, 3)
    assert(solveForSatisfiability(rc1) === None)
    assert(solveForSatisfiability(rc2) === None)

    val nd1: List[Formula] = List(true, false, true)
    val nd2: List[Formula] = List(true, true, false)
    val rd = lessEquals(nd1, nd2)
    assert(solveForSatisfiability(rd) != None)
  }

  test("less equals between constants of different length is working") {
    val na1: List[Formula] = List(true, false, true)
    val na2: List[Formula] = List(true, true)
    val ra = lessEquals(na1, na2)
    assert(solveForSatisfiability(ra) === None)

    val nb1: List[Formula] = List(true)
    val nb2: List[Formula] = List(true, true)
    val rb = lessEquals(nb1, nb2)
    assert(solveForSatisfiability(rb) != None)

    val nc1: List[Formula] = List(true, false, true)
    val nc2: List[Formula] = List(false, true, false, false)
    val rc1 = lessEquals(nc1, nc2)
    val rc2 = lessEqualsConst(nc1, 3)
    assert(solveForSatisfiability(rc1) === None)
    assert(solveForSatisfiability(rc2) === None)

    val nd1: List[Formula] = List(false, true, false, true)
    val nd2: List[Formula] = List(true, true, false)
    val rd = lessEquals(nd1, nd2)
    assert(solveForSatisfiability(rd) != None)
  }

  test("less equals between symbolic numbers") {
    val x1 = boolVar()
    val x2 = boolVar()
    val x3 = boolVar()
    val y1 = boolVar()
    val y2 = boolVar()
    val y3 = boolVar()

    val f1 = lessEquals(List(x1), List(y1))
    val thm1 = f1 iff !(x1 && !y1)
    assert(solveForSatisfiability(!thm1) === None)

    val f2 = lessEquals(List(x3, x2, x1), List(y3, y2, y1))
    solveForSatisfiability(f2).foreach(model => {
      val n1 = List(model(x3), model(x2), model(x1))
      val n2 = List(model(y3), model(y2), model(y1))
      assert(binary2int(n1) <= binary2int(n2))
    })
  }

  def checkNumber(res: List[Formula], context: Set[Formula], expected: List[Boolean]): Boolean = {

    val rres = res.reverse

    val digitsOk = and(rres.zip(expected.reverse).map(p => p._1 iff p._2):_*)
    val leadingZeros = !or(rres.drop(expected.size):_*)

    val theorem = digitsOk && leadingZeros && and(context.toSeq:_*)

    expected.size <= res.size && solveForSatisfiability(theorem) != None
  }

  test("sum of one bit numbers") {
    val na1: List[Formula] = List(true)
    val na2: List[Formula] = List(true)
    val (ra, ca) = sum(na1, na2)
    assert(checkNumber(ra, ca, List(true, false)))
    assert(!checkNumber(ra, ca, List(true, true)))
    assert(!checkNumber(ra, ca, List(true)))
    assert(!checkNumber(ra, ca, List(false)))

    val nb1: List[Formula] = List(true)
    val nb2: List[Formula] = List(false)
    val (rb, cb) = sum(nb1, nb2)
    assert(checkNumber(rb, cb, List(true)))

    val nc1: List[Formula] = List(false)
    val nc2: List[Formula] = List(true)
    val (rc, cc) = sum(nc1, nc2)
    assert(checkNumber(rc, cc, List(true)))

    val nd1: List[Formula] = List(false)
    val nd2: List[Formula] = List(false)
    val (rd, cd) = sum(nd1, nd2)
    assert(checkNumber(rd, cd, List(false)))
  }

  test("sum is computing correct result for two constants") {
    val na1: List[Formula] = List(false, true)
    val na2: List[Formula] = List(true, true)
    val (ra, ca) = sum(na1, na2)
    assert(checkNumber(ra, ca, List(true, false, false)))

    val nb1: List[Formula] = List(true, false, true)
    val nb2: List[Formula] = List(false, true, true)
    val (rb, cb) = sum(nb1, nb2)
    assert(checkNumber(rb, cb, List(true, false, false, false)))

    val nc1: List[Formula] = List(true, false, true)
    val nc2: List[Formula] = List(true, false, true)
    val (rc, cc) = sum(nc1, nc2)
    assert(checkNumber(rc, cc, List(true, false, true, false)))
  }


  test("sum is working with numbers of different length") {
    val na1: List[Formula] = List(true)
    val na2: List[Formula] = List(true, true)
    val (ra, ca) = sum(na1, na2)
    assert(checkNumber(ra, ca, List(true, false, false)))

    val nb1: List[Formula] = List(true, false, true)
    val nb2: List[Formula] = List(true, true)
    val (rb, cb) = sum(nb1, nb2)
    assert(checkNumber(rb, cb, List(true, false, false, false)))

    val nc1: List[Formula] = List(true)
    val nc2: List[Formula] = List(true, false)
    val (rc, cc) = sum(nc1, nc2)
    assert(checkNumber(rc, cc, List(true, true)))

  }

  test("Symbolic sum to find components") {

    val x1 = boolVar()
    val x2 = boolVar()
    val x3 = boolVar()
    val x4 = boolVar()
    val y1 = boolVar()
    val y2 = boolVar()
    val y3 = boolVar()
    val y4 = boolVar()

    val (s1, c1) = sum(List(x1,x2,x3), List(y1,y2,y3))
    val f1 = (s1(0) iff true) && (s1(1) iff false) && (s1(2) iff true) && (s1(3) iff true)
    val m1 = solveForSatisfiability(f1 && and(c1.toSeq:_*))
    assert(!m1.isEmpty)
    m1.foreach(m => {
      val n1 = binary2int(List(m(x1), m(x2), m(x3)))
      val n2 = binary2int(List(m(y1), m(y2), m(y3)))
      assert(n1 + n2 === 11)
    })

    val (s2, c2) = sum(List(x1,x2,x3,x4), List(y1,y2,y3,y4))
    val f2 = (s2(0) iff true) && (s2(1) iff true) && (s2(2) iff false) && (s2(3) iff true) && (s2(4) iff false)
    val m2 = solveForSatisfiability(f2 && and(c2.toSeq:_*))
    assert(!m2.isEmpty)
    m2.foreach(m => {
      val n1 = binary2int(List(m(x1), m(x2), m(x3), m(x4)))
      val n2 = binary2int(List(m(y1), m(y2), m(y3), m(y4)))
      assert(n1 + n2 === 26)
    })
  }

}
