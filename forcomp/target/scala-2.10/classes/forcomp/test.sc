package forcomp


object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  "helLo".toLowerCase().groupBy((c: Char) => c).toList map { case (c,cs) => (c, cs.length)}
                                                  //> res0: List[(Char, Int)] = List((e,1), (h,1), (l,2), (o,1))
 	Anagrams.sentenceOccurrences(List("heLlo","world"))
                                                  //> res1: forcomp.Anagrams.Occurrences = List((d,1), (e,1), (h,1), (l,3), (o,2),
                                                  //|  (r,1), (w,1))
 	//Anagrams.dictionary
 	
 	//Anagrams.dictionaryByOccurrences
 	
 	//Anagrams.wordAnagrams("sahcdjcoisnmsaoi")
 	
 	//Anagrams.combinations(List(('a', 2),('c',1)))
 	
 	List(('a', 2), ('b', 2)).toMap            //> res2: scala.collection.immutable.Map[Char,Int] = Map(a -> 2, b -> 2)
 	Map('a' -> 2, 'b' -> 2).toList            //> res3: List[(Char, Int)] = List((a,2), (b,2))
 	Anagrams.subtract(List(('a', 2), ('b', 2)),List(('a', 1)))
                                                  //> res4: forcomp.Anagrams.Occurrences = List((a,1), (b,2))
 	
 	Anagrams.sentenceAnagrams(List("I", "love", "you"))
                                                  //> res5: List[forcomp.Anagrams.Sentence] = List(List(you, Io, Lev), List(you, L
                                                  //| ev, Io), List(you, olive), List(Io, you, Lev), List(Io, Lev, you), List(Lev,
                                                  //|  you, Io), List(Lev, Io, you), List(olive, you))
 	
}