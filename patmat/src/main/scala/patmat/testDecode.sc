package patmat

import patmat.Huffman

object testDecode {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  Huffman.decode(Huffman.frenchCode, Huffman.secret)
                                                  //> res0: List[Char] = List(h, u, f, f, m, a, n, e, s, t, c, o, o, l)
  
  

}