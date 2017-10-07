package com.sepoe._99

/**
 *
P21 (*) Insert an element at a given position into a list.
    Example:

    scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
    res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
 */
object P21 {

def main(args: Array[String]): Unit = {	
	  val list = List('a, 'b, 'c, 'd)
	  
	 val n = 1
	  
	  funcs[Symbol].foreach{ f => println(f('new, n,list)) }	  
	   	   
	   
  }
  
  def funcs[A] = Array[(A, Int, List[A]) => List[A]](
		  insertAt1 _
		  , insertAt2 _
		  , insertAt3 _
		  , insertAt _
  		)

  		
  def insertAt1[A](x:A, n:Int, list:List[A]):List[A] = {
	  val (l1,l2) = list.splitAt(n)
	  l1 ::: (x :: l2)
  }
  
  def insertAt2[A](x:A, n:Int, list:List[A]):List[A] = {
	require(0 <= n , "n must be in list range")
	def insertRec(k:Int, l1:List[A], l2:List[A]):List[A] = (k, l2) match {
		case (i , ys) if i == n || ys == Nil => l1.reverse ::: (x :: ys)
		case (_, y :: ys) => insertRec(k+1, y :: l1, ys)		
	}
	
	insertRec(0,Nil, list)
  } 

	def insertAt3[A](x:A, n:Int, list:List[A]):List[A] = {
	  	if(list == Nil) List(x)
	  	else (list.zipWithIndex) flatMap{case (y,i) => if(i == n) List(x,y) else List(y) }
	}
  
	// sample solution
  def insertAt[A](e: A, n: Int, ls: List[A]): List[A] = ls.splitAt(n) match {
    case (pre, post) => pre ::: e :: post
  }
	

}