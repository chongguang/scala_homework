package week2

object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  object sim extends Circuits with Parameters
  import sim._
  val in1, in2, sum, carry = new Wire             //> in1  : week2.test.sim.Wire = week2.Gates$Wire@68beaed3
                                                  //| in2  : week2.test.sim.Wire = week2.Gates$Wire@3ec7d45e
                                                  //| sum  : week2.test.sim.Wire = week2.Gates$Wire@31478785
                                                  //| carry  : week2.test.sim.Wire = week2.Gates$Wire@4f124609

  halfAdder(in1, in2, sum, carry)
  probe("sum", sum)                               //> sum 0 new-value = false
  probe("carry", carry)                           //> carry 0 new-value = false
  
  in1 setSignal true
  run()                                           //> *** simulation started, time = 0 ***
                                                  //| sum 5 new-value = true
                                                  //| sum 10 new-value = false
                                                  //| sum 10 new-value = true
          
  in2 setSignal true
  run()                                           //> *** simulation started, time = 10 ***
                                                  //| carry 13 new-value = true
                                                  //| sum 18 new-value = false
  in1 setSignal false
  run()                                           //> *** simulation started, time = 18 ***
                                                  //| carry 21 new-value = false
                                                  //| sum 26 new-value = true
         
}