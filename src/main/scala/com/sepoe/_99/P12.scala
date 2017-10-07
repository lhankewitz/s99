package com.sepoe._99

/**
 *
Decode a run-length encoded list.
    Given a run-length code list generated as specified in problem P10, construct its uncompressed version.

    Example:

    scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
    res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
 */
object P12 {

  def main(args: Array[String]): Unit = {  
	  val list = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
	  
	  
	  funcs[Symbol].foreach{ f => println(f(list)) }	  
  }
  
  def funcs[A] = Array[List[(Int, A)] => List[A]](
		  decode _
		  , decode2 _
		  , decode3 _
  		)

   		
  	def decode[A](list: List[(Int, A)]):List[A] = 
  		list.flatMap{case (num, item) => for(i <- 1 to num) yield item}
  
    def decode2[A](list: List[(Int, A)]):List[A] = 
    	//for((num, item) <- list; i <- 1 to num) yield item
    	for(t <- list; i <- 1 to t._1) yield t._2
    	

    def decode3[A](list: List[(Int, A)]):List[A] = 
    	list.flatMap{t => List.fill(t._1){t._2}}
    	// list.flatMap{t => List.make(t._1, t._2)} // make this is deprecated in 2.8  
    
}