package com.sepoe._99

/**
 *
P20 (*) Remove the Kth element from a list.
    Return the list and the removed element in a Tuple. Elements are numbered from 0.

    Example:

    scala> removeAt(1, List('a, 'b, 'c, 'd))
    res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
 */
object P20 {

def main(args: Array[String]): Unit = {	
	  val list = List('a, 'b, 'c, 'd)
	  
	 val n = 1
	  
	  funcs[Symbol].foreach{ f => println(f(n,list)) }  	   
	   
 }
  
  def funcs[A] = Array[(Int, List[A]) => (List[A], A)](
		  removeAt1 _
		  , removeAt2 _
		  , removeAt3 _
		  , removeAt4 _
		  , removeAt5 _
  		)

  		
  def removeAt1[A](k:Int, list:List[A]):(List[A], A) = (list.take(k-1) ::: list.drop(k), list(k))
 
  def removeAt2[A](k:Int, list:List[A]):(List[A], A) = {
	  val (s,t) = list.splitAt(k-1)
	  (s ::: t.tail, list(k))
  }
  
	def removeAt3[A](k:Int, list:List[A]):(List[A], A) = {
		val l:List[A] = (for(i <- 0 until list.length; if(i != k))yield list(i)).toList
		(l, list(k))
	}
	
	def removeAt4[A](k:Int, list:List[A]):(List[A], A) = {
		((list.zipWithIndex).filter {_._2 != k}.map{ _._1}, list(k))
	}
	
	def removeAt5[A](k:Int, list:List[A]):(List[A], A) = {
	require(k >= 0, "k must not be less than 0")
	
		def splitAt(n:Int, l1:List[A], l2:List[A]):(List[A], A) = (n, l2) match { 
			case (_, _) if (n == k) => (l1.reverse ::: l2.tail, l2.head)
			case (_, _) => splitAt(n+1, l2.head :: l1, l2.tail)
		}
		
		splitAt(0, Nil, list)
	}
	
	
 
	//sample solution 1
	def removeAt[A](n: Int, ls: List[A]): (List[A], A) = ls.splitAt(n) match {
	    case (Nil, _) if n < 0 => throw new NoSuchElementException
	    case (pre, e :: post)  => (pre ::: post, e)
	    case (pre, Nil)        => throw new NoSuchElementException
	  }

  // sample solution 2 - Alternate, with fewer builtins.
  def removeAt0[A](n: Int, ls: List[A]): (List[A], A) =
    if (n < 0) throw new NoSuchElementException
    else (n, ls) match {
      case (_, Nil) => throw new NoSuchElementException
      case (0, h :: tail) => (tail, h)
      case (_, h :: tail) => {
        val (t, e) = removeAt(n - 1, ls.tail)
        (ls.head :: t, e)
      }
    }
	

}