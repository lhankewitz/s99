package com.sepoe._99

/**
 *
P17 (*) Split a list into two parts.
    The length of the first part is given. Use a Tuple for your result.

    Example:

    scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    
 */
object P17 {

 def main(args: Array[String]): Unit = {	
	  val list = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
	  
	 val n = 10
	  
	  funcs[Symbol].foreach{ f => println(f(n,list)) }	  
	   
	  println(split3(n,Nil))
	  println(split3(n,Nil)) 
  }
  
  def funcs[A] = Array[(Int,List[A]) => (List[A], List[A])](
		  split1 _
		  , split2 _
		  , split3 _
  		)

  		
  def split1[A](n:Int, list:List[A]):(List[A], List[A]) = (list.take(n), list.drop(n))
  
  def split2[A](n:Int, list:List[A]):(List[A], List[A]) = list.splitAt(n)
  
  def split3[A](n:Int, list:List[A]):(List[A], List[A]) = {
	  def localSplit(m:Int, l1:List[A], l2:List[A]):(List[A], List[A]) = 
	 	  if (m < n && l2 != Nil) localSplit(m + 1, l2.head :: l1, l2.tail)
	 	  else (l1.reverse , l2)
	 localSplit(0, List[A](), list)
  }
  
  
}