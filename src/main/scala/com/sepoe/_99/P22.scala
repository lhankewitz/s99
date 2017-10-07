package com.sepoe._99

/**
 *
P22 (*) Create a list containing all integers within a given range.
    Example:

    scala> range(4, 9)
    res0: List[Int] = List(4, 5, 6, 7, 8, 9)
 */
object P22 {

def main(args: Array[String]): Unit = {	

	  
	 val n = 4
	 val m = 9
	  
	  funcs.foreach{ f => println(f(n,m)) }	  
	   	   
	   
  }
  
  def funcs = Array[(Int, Int) => List[Int]](
		  range1 _
		  , range2 _
		  , range3 _
  		)

  		
  def range1(n:Int, m:Int):List[Int] = (n to m).toList

  def range2(n:Int, m:Int):List[Int] = {
	  def tolist(i:Int, list:List[Int]):List[Int] = 
	 	  if(i >= n && i <= m) tolist(i+1, i :: list) 
	 	  else list.reverse
	   
	 tolist(n, Nil)
  }
  
  
	def range3(n:Int, m:Int):List[Int] = {
		require(n <= m, "n must not be greater than m")
		if(n <= m) n :: range3(n+1, m)
		else Nil
	}
  
	// sample solutions
	// -----------------------------------
  def rangeBuiltin(start: Int, end: Int): List[Int] = List.range(start, end + 1)

  // Recursive.
  def rangeRecursive(start: Int, end: Int): List[Int] =
    if (end < start) Nil
    else start :: rangeRecursive(start + 1, end)

  // Tail recursive.
  def rangeTailRecursive(start: Int, end: Int): List[Int] = {
    def rangeR(end: Int, result: List[Int]): List[Int] = {
      if (end < start) result
      else rangeR(end - 1, end :: result)
    }
    rangeR(end, Nil)
  }

  // The classic functional approach would be to use `unfoldr`, which Scala
  // doesn't have.  So we'll write one and then use it.
  def unfoldRight[A, B](s: B)(f: B => Option[(A, B)]): List[A] =
    f(s) match {
      case None         => Nil
      case Some((r, n)) => r :: unfoldRight(n)(f)
    }
  def rangeFunctional(start: Int, end: Int): List[Int] =
    unfoldRight(start) { n =>
      if (n > end) None
      else Some((n, n + 1))
    }
	
	
}