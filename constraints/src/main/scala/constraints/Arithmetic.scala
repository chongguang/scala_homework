package constraints

import cafesat.api.API._
import scala.annotation.tailrec

/**
  * This object contains utility functions for encoding
  * some arithmetic constraints as boolean ones
  */
object Arithmetic {

  /**
   * Transforms a positive integer in binary form into its base 2 representation.
   * The `head` element of the input list contains the most
   * significant bit (the list is in big-endian form).
   */
  def binary2int(n: List[Boolean]): Int = {  
    @tailrec
    def iter(n: List[Boolean], acc: Int): Int = n match {
      case Nil => acc
      case x::xs => if(x==true) {iter(xs, acc*2 + 1)} else {iter(xs, acc*2)}
    }    
    iter(n,0)
  }

  /**
   * Encodes an positive integer number into base 2.
   * The `head` element of the resulting list contains the most significant
   * bit, and cannot be `false`.
   */
  def int2binary(n: Int): List[Boolean] = {
    @tailrec
    def iter(n: Int, acc: List[Boolean]): List[Boolean] = (n, acc) match {
      case (0, Nil) => false::Nil
      case (0, _) => acc
      case (n, _) => if(n%2 == 1) {iter(n/2, true::acc)} else {iter(n/2, false::acc)}
    }    
    iter(n, Nil)    
  }


  /**
   * This function takes two arguments, both representing positive
   * integers encoded as lists of propositional variables. It returns
   * a formula that represents a boolean circuit that constraints
   * `n1` to be less than or equal to `n2`
   */
  def lessEquals(n1: List[Formula], n2: List[Formula]): Formula = {
    
    def filterFalseAtBeginning(n: List[Formula]): List[Formula] = {
      if(n.size == 0) Nil
      else if (n.head == bool2formula(true)) n
      else filterFalseAtBeginning(n.tail)
    }
    
    def iter(n1: List[Formula], n2: List[Formula]): Formula = {
      if (n1.size == 0) bool2formula(true)
      else if (n1.head == n2.head) iter(n1.tail, n2.tail)
      else if (n1.head == bool2formula(true) && n2.head == bool2formula(false)) bool2formula(false)
      else bool2formula(true)
    }
    
    val n1f = filterFalseAtBeginning(n1)
    val n2f = filterFalseAtBeginning(n2)
    
    if (n1f.size<n2f.size) {
      bool2formula(true)
    } else if (n1f.size>n2f.size){
      bool2formula(false)
    } else {
      iter(n1f, n2f)
    }
    
  }

  def xor(x: Formula, y: Formula): Formula = (x && !y) || (y && !x)
  def carryOut(x: Formula, y: Formula, cin: Formula): Formula = (x && y) || (cin && xor(x, y))  
  def sum(x: Formula, y: Formula, cin: Formula): Formula =  xor(x, xor(y, cin))
  
  /**
   * This function takes two arguments, both representing positive integers
   * encoded as lists of propositional variables. It returns a pair.
   *
   * The first element of the pair is a `List[Formula]`, and it represents
   * the resulting binary number.
   * The second element is a set of intermediate constraints that are created
   * along the way.
   *
   */
  def sum(n1: List[Formula], n2: List[Formula]): (List[Formula], Set[Formula]) = {
    @tailrec
    def toSameSize(n1: List[Formula], n2: List[Formula]): (List[Formula], List[Formula]) = {
      if(n1.size < n2.size) toSameSize(bool2formula(false)::n1, n2)
      else if (n1.size > n2.size) toSameSize(n1, bool2formula(false)::n2)
      else (n1, n2)
    }

    def reverseList2[A](list : List[A]) : List[A] = {
      def rlRec[A](result : List[A], list : List[A]) : List[A] = {
        list match {
          case Nil => result
          case (x :: xs) => { rlRec(x :: result, xs) }
        }
      }
      rlRec(Nil, list)
    }
    
    def iter(n1: List[Formula], n2: List[Formula], cin: Formula, accSum: List[Formula]): (List[Formula], Formula) = (n1, n2) match {
      case(x::Nil, y::Nil) => {
        val cout = carryOut(x, y, cin)
        val s = sum(x, y, cin)
        (cout::s::accSum, cout)        
      }
      
      case(x::xs, y::ys) => {
        val cout = carryOut(x, y, cin)
        val s = sum(x, y, cin)
        iter(xs, ys, cout, s::accSum)        
      }      
    }
    
    val (n1b, n2b) = toSameSize(n1, n2)
    val n1r = reverseList2(n1b)
    val n2r = reverseList2(n2b)
    
    (iter(n1r, n2r,  bool2formula(false), Nil)._1, Set())
  }


  /**
   * A helper function that creates a less-equals formula
   * taking an integer and a formula as parameters
   */
  def lessEqualsConst(cst: Int, n: List[Formula]): Formula = {
    lessEquals(int2binary(cst), n)
  }

  /**
   * A helper function that creates a less-equals formula
   * taking a formula and an integer as parameters
   */
  def lessEqualsConst(n: List[Formula], cst: Int): Formula = {
    lessEquals(n, int2binary(cst))
  }

}
