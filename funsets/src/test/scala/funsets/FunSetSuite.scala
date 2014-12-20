package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    
    val us1s2 = union(s1,s2)
    val us1s1 = union(s1,s1)
    val us1s2s3 = union(us1s2,s3)
    
    val is1s2 = intersect(s1,s2)
    val is1us1s2 = intersect(s1,us1s2)
    val is1s1 = intersect(s1,s1)
    
    val ds1s1 = diff(s1,s1)
    val ds1s2 = diff(s1,s2)
    val d_us1s2_s2 = diff(us1s2,s2)
    val d_s1_us1s2 = diff(s1,us1s2)
    
    def biggerThan2(value: Int): Boolean = {
      if (value > 2) true 
      else false
    }
    
    def smallerThan9999(value: Int): Boolean = {
      if (value < 9999) true 
      else false
    }
    
    def plus2(value: Int): Int = value + 2    
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  
  /**
   * Here are the tests written by Chongguang
   * Tests for singletonSet 
   */
  test("Tests of function singletonSet()") {
    assert(singletonSet(1)(1))
    assert(singletonSet(0)(0))
    assert(singletonSet(-3)(-3))
    
    assert(!singletonSet(1)(0))
    assert(!singletonSet(1)(-1))
    assert(!singletonSet(1)(2))
    
    assert(!singletonSet(-1)(0))
    assert(!singletonSet(-1)(-3))
    assert(!singletonSet(-1)(2))
    
    assert(!singletonSet(0)(10))
    assert(!singletonSet(0)(-3))    
  }
  
  test("Tests of the function contains()"){
    new TestSets {
      assert(contains(s1,1))
      assert(!contains(s1,2))
      assert(!contains(s1,0))
      assert(!contains(s1,-2))
    }
  }
  
  test("Tests of the function union()"){
    new TestSets{
      assert(contains(us1s2,1))
      assert(contains(us1s2,2))
      
      assert(!contains(us1s2,0))
      assert(!contains(us1s2,-1))
      assert(!contains(us1s2,5))
      
      assert(contains(us1s2s3,1))
      assert(contains(us1s2s3,2))
      assert(contains(us1s2s3,3))
      
      assert(!contains(us1s2s3,0))
      assert(!contains(us1s2s3,-6))
      assert(!contains(us1s2s3,6))
      
      assert(contains(us1s1,1))
      
      assert(!contains(us1s1,0))
      assert(!contains(us1s1,-1))
      assert(!contains(us1s1,5))
    }
  }
  
  test("Tests of the function intersect()"){
    new TestSets{
      assert(!contains(is1s2,1))
      assert(!contains(is1s2,2))
      assert(!contains(is1s2,3))
      assert(!contains(is1s2,0))
      assert(!contains(is1s2,-4))
      
      assert(contains(is1us1s2,1))
      assert(!contains(is1us1s2,2))
      assert(!contains(is1us1s2,3))
      assert(!contains(is1us1s2,0))
      assert(!contains(is1us1s2,-1))  
      
      assert(contains(is1s1,1))
      assert(!contains(is1s1,2))
      assert(!contains(is1s1,3))
      assert(!contains(is1s1,0))
      assert(!contains(is1s1,-4))
    }
  }
  
  test("Tests of the function diff()"){
    new TestSets{
      assert(!contains(ds1s1,1))
      assert(!contains(ds1s1,2))
      assert(!contains(ds1s1,0))
      assert(!contains(ds1s1,-1))
      
      assert(contains(ds1s2,1))
      assert(!contains(ds1s2,2))
      assert(!contains(ds1s2,0))
      assert(!contains(ds1s2,-1))
      
      assert(contains(d_us1s2_s2,1))
      assert(!contains(d_us1s2_s2,2))
      assert(!contains(d_us1s2_s2,0))
      assert(!contains(d_us1s2_s2,-1))
      
      assert(!contains(d_s1_us1s2,1))
      assert(!contains(d_s1_us1s2,2))
      assert(!contains(d_s1_us1s2,0))
      assert(!contains(d_s1_us1s2,-1))
    }
  }
  
  test("Tests of the function filter()"){
    new TestSets{
      assert(contains(filter(us1s2s3,biggerThan2),3))
      assert(!contains(filter(us1s2s3,biggerThan2),1))
      assert(!contains(filter(us1s2s3,biggerThan2),2))
      assert(!contains(filter(us1s2s3,biggerThan2),6))
      assert(!contains(filter(us1s2s3,biggerThan2),0))
      assert(!contains(filter(us1s2s3,biggerThan2),-4))
    }    
  }
  
  test("Tests of the function forall()"){
    new TestSets{
      assert(forall(us1s2s3,smallerThan9999))
      assert(!forall(us1s2s3,biggerThan2))
    }
  }
  
  test("Tests of the function exists()"){
    new TestSets{
      assert(exists(us1s2s3,smallerThan9999))
      assert(exists(us1s2s3,biggerThan2))
      assert(!exists(s1,biggerThan2))
    }
  }
  
  test("Tests of the function map()"){
    new TestSets{
      assert(contains(map(s1,plus2),3))
      assert(!contains(map(s1,plus2),1))
      assert(!contains(map(s1,plus2),2))
      assert(!contains(map(s1,plus2),0))
      assert(!contains(map(s1,plus2),-2))
      
      assert(contains(map(us1s2,plus2),3))
      assert(contains(map(us1s2,plus2),4))
      assert(!contains(map(us1s2,plus2),2))
      assert(!contains(map(us1s2,plus2),1))
      assert(!contains(map(us1s2,plus2),-2))
      assert(!contains(map(us1s2,plus2),0))
    }
  }
}
