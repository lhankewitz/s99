package com.sepoe._99_2


import P10._

/**
 *
Modified run-length encoding.
    Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.

    Example:

    scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
 */
object ModifiedRunLengthEncoding {

  def main(args: Array[String]): Unit = {
	  
	  val list = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
	  
	  funcs[Symbol].foreach{ f => println(f(list)) }
	  
  }
  
  def funcs[A] = Array[List[A] => List[Any]](
		encodeModified _  
		, encodeModified2 _
		, encodeModified3 _
  		)
  
  def encodeModified[A](list:List[A]):List[Any] = encode(list).map{ e => if(e._1 == 1) e._2 else e}
  
  /**
	By convention Right is for the correct version and Left for failures. =>
	Either cond (<test for Right, Right, Left):Either[]<type of Left>, <type of Right>]
 */
def encodeModified2[A](list:List[A]):List[Either[A, (Int, A)]] = {
	  encode(list).map{ case (num, s) => Either cond (num != 1, (num, s),  s) }
  }
  
  /**
	By convention Right is for the correct version and Left for failures.
 */
def encodeModified3[A](list:List[A]):List[Either[A, (Int, A)]] = {
		  encode(list).map{ t => if(t._1 == 1) Left(t._2) else Right(t) }
  }

}