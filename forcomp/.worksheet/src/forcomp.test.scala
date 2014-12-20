package forcomp


object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(75); 
  println("Welcome to the Scala worksheet");$skip(92); val res$0 = 
  "helLo".toLowerCase().groupBy((c: Char) => c).toList map { case (c,cs) => (c, cs.length)};System.out.println("""res0: List[(Char, Int)] = """ + $show(res$0));$skip(54); val res$1 = 
 	Anagrams.sentenceOccurrences(List("heLlo","world"));System.out.println("""res1: forcomp.Anagrams.Occurrences = """ + $show(res$1));$skip(202); val res$2 = 
 	//Anagrams.dictionary
 	
 	//Anagrams.dictionaryByOccurrences
 	
 	//Anagrams.wordAnagrams("sahcdjcoisnmsaoi")
 	
 	//Anagrams.combinations(List(('a', 2),('c',1)))
 	
 	List(('a', 2), ('b', 2)).toMap;System.out.println("""res2: scala.collection.immutable.Map[Char,Int] = """ + $show(res$2));$skip(33); val res$3 = 
 	Map('a' -> 2, 'b' -> 2).toList;System.out.println("""res3: List[(Char, Int)] = """ + $show(res$3));$skip(61); val res$4 = 
 	Anagrams.subtract(List(('a', 2), ('b', 2)),List(('a', 1)));System.out.println("""res4: forcomp.Anagrams.Occurrences = """ + $show(res$4));$skip(57); val res$5 = 
 	
 	Anagrams.sentenceAnagrams(List("I", "love", "you"));System.out.println("""res5: List[forcomp.Anagrams.Sentence] = """ + $show(res$5))}
 	
}
