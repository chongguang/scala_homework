package interpreter

object Main extends App {

  import java.io._
  
  print("lisp> ")
  
  def stdIn: BufferedReader = new BufferedReader(new InputStreamReader(System.in))
  var input = stdIn.readLine()

  while (!(input.equalsIgnoreCase("exit"))) {
    try {
    println(Lisp.evaluate(input))
    }
    catch {
      case e: Error => println(e.getMessage())
      case e: Exception => println(e.getMessage())
    }
    print("lisp> ")
    input = stdIn.readLine()
  }
}

object LispCode {
  // TODO: implement the function `concat` in Lisp. Write it as a String, and test it in your REPL
  val concat = """
  def (concat L1 L2) (if (null? L1) L2 (cons (car L1) (concat (cdr L1) L2)))
  """
  // TODO: implement the function `reverse` in Lisp. Write it as a String, and test it in your REPL
  val reverse = """
  def (reverse L) (if (null? L) nil (concat (reverse (cdr L)) (cons (car L) nil)))
  """
}