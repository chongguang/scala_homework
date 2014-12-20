package interpreter

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Lisp._

@RunWith(classOf[JUnitRunner])
class LispSuite extends FunSuite {

  val expr1 = "(+ 1 2)"
  test("addition") {
    assert(string2lisp(expr1).toString === "List('+, 1, 2)")
    assert(evaluate(expr1) === 3)
  }

  def testLisp(testName: String, expectedResult: Data, expr: String) {
    test(testName + " / evaluate") {
      assert(expectedResult === evaluate(expr))
    }
  }

  testLisp("defSugar2", 3, "(def (add a b) (+ a b) (add 1 2))")

  testLisp("caseSugar2", 3, "(case 1 (1 3) (else 4))")
  testLisp("caseSugar3", 4, "(case 2 (1 3) (else 4))")

  // concat and reverse tests
  testLisp("concatLisp1", List(1,2), "(" + LispCode.concat + " (concat (cons 1 nil) (cons 2 nil)))")

  testLisp("reverseLisp1", List(2,1), "(" + LispCode.concat + "(" + LispCode.reverse +" (reverse (concat (cons 1 nil) (cons 2 nil)))))")
}
