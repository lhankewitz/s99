package com.sepoe._99

/**
P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
    In how many ways can a committee of 3 be chosen from a group of 12 people? 
    We all know that there are C(12,3) = 220 possibilities (C(N,K) 
    denotes the well-known binomial coefficient). For pure mathematicians, 
    this result may be great. But we want to really generate all the possibilities.

    Example:

    scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
    res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
 */
object P26 {

def main(args: Array[String]): Unit = {	
	  val list = List('a, 'b, 'c, 'd, 'e, 'f)
//	  val list = List('a, 'b, 'c, 'd, 'e, 'f, 'h, 'i, 'j, 'k, 'l, 'm)
	  
	 val n = 3
	  
	  funcs[Symbol].foreach{ f => { 
	 	  val result = f(n,list)
	 	  println(result.length + " number of combinations.")
	 	  println(result)
	 	  }   	   
	  }
	   
 } 
  
  def funcs[A] = Array[(Int, List[A]) => List[List[A]]](
		  combinations1 _
		  , combinations2 _

  		)
		
  		

def combinations1[A](k:Int, l:List[A]):List[List[A]] = {
	require(k> 0, "k must not be <= 0") 
	if(k == 1) l.map { x => List(x)}
	else l.flatMap {
		x => combinations1(k-1, (l.reverse.takeWhile{_ != x}).reverse).map{ x :: _}
	}
}
def combinations2[A](k:Int, l:List[A]):List[List[A]] = {
	require(k>= 0, "k must not be < 0") 
	if(k == 0) List(Nil)
	else l.flatMap {
		x => combinations1(k-1, (l.reverse.takeWhile{_ != x})).map{ x :: _}
	}
}
  
  
  // flatMapSublists is like list.flatMap, but instead of passing each element
  // to the function, it passes successive sublists of L.
  def flatMapSublists[A,B](ls: List[A])(f: (List[A]) => List[B]): List[B] = 
    ls match {
      case Nil => Nil
      case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }

  def combinations[A](n: Int, ls: List[A]): List[List[A]] =
    if (n == 0) List(Nil)
    else flatMapSublists(ls) { sl =>
      combinations(n - 1, sl.tail) map {sl.head :: _}
    }
  

}