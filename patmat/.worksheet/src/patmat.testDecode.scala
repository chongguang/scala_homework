package patmat

import patmat.Huffman

object testDecode {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(102); 
  println("Welcome to the Scala worksheet");$skip(56); val res$0 = 
  
  Huffman.decode(Huffman.frenchCode, Huffman.secret);System.out.println("""res0: List[Char] = """ + $show(res$0))}
  
  

}
