package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


/**
 * @author chongguang
 *
 */
@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  import Main.pascal
  test("pascal: col=0,row=2") {
    assert(pascal(0,2) === 1)
  }

  test("pascal: col=1,row=2") {
    assert(pascal(1,2) === 2)
  }

  test("pascal: col=1,row=3") {
    assert(pascal(1,3) === 3)
  }
  
  /**
   * Personnel unit tests
   */
  test("pascal: col=0,row=0") {
    assert(pascal(0,0) === 1)
  }

  test("pascal: col=-1,row=2") {
    intercept[IllegalArgumentException ] { pascal(-1,2) }
  }

  test("pascal: col=1,row=-2") {
    intercept[IllegalArgumentException ] { pascal(1,-2) }
  }

  test("pascal: col=-1,row=-2") {
    intercept[IllegalArgumentException ] { pascal(-1,-2) }
  }

  test("pascal: col=8,row=3") {
    intercept[IllegalArgumentException ] { pascal(8,3) }
  }

  test("pascal: col=2,row=10") {
    assert(pascal(2,10) === 45)
  }
}
