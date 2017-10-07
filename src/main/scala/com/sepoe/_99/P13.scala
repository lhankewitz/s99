package com.sepoe._99_2

/**
 *
P13 (**) Run-length encoding of a list (direct solution).
    Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.

    Example:

    scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
 */
object P13 {

  def main(args: Array[String]): Unit = {	
	  val list = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
	  
	  
	  funcs[Symbol].foreach{ f => println(f(list)) }	  
  }
  
  def funcs[A] = Array[List[A] => List[(Int, A)]](
		  encodeDirect _
  		)
  		
  	/**
	* wow did the sample solution by myself :-))
	   */
	  def encodeDirect[A](list:List[A]):List[(Int, A)] = 
  		if (list.isEmpty) Nil
  		else {
  			val (prefix, suffix) = list.span(_ == list.head)
  			(prefix.length, prefix.head) :: encodeDirect(suffix)
  		}
  
}