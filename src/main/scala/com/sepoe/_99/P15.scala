package com.sepoe._99_2

/**
 *
P15 (**) Duplicate the elements of a list a given number of times.
    Example:

    scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
    res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
 */
object P15 {

def main(args: Array[String]): Unit = {	
	  val list = List('a, 'b, 'c, 'c, 'd)
	  
	  
	  funcs[Symbol].foreach{ f => println(f(3,list)) }	  
  }
  
  def funcs[A] = Array[(Int,List[A]) => List[A]](
		  duplicate1N _
		  , duplicate2N _
		  , duplicateN _

  		)

  def duplicate1N[A](n:Int, list:List[A]):List[A] = 
	  list.flatMap{s => for(i <- 1 to n) yield s}	
  
  def duplicate2N[A](n:Int, list:List[A]):List[A] = 
	list.flatMap{s => List.fill(n){s}}	
  
  /**
	sample solution
 */
  def duplicateN[A](n:Int, list:List[A]):List[A] = 
	  list.flatMap{List.fill(n)(_)}	
	
}