package com.sepoe._99

/**
 *
P19 (**) Rotate a list N places to the left.
    Examples:

    scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

    scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
 */
object P19 {

 def main(args: Array[String]): Unit = {	
	  val list = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
	  
	 val n = 4
	  
	  funcs[Symbol].foreach{ f => println(f(n,list)) }	  
	   
	 val m = -2
	  
	  funcs[Symbol].foreach{ f => println(f(m,list)) }	  	   
	   
  }
  
  def funcs[A] = Array[(Int, List[A]) => List[A]](
		  rotate1 _
		  , rotate _
  		)

  		
  def rotate1[A](n:Int, list:List[A]):List[A] = 
	  if (n >= 0) list.drop(n) ::: list.take(n)
	  else rotate1(-n, list.reverse).reverse


  /**
	sample solution
 */
def rotate[A](n: Int, ls: List[A]): List[A] = {
    val nBounded = if (ls.isEmpty) 0 else n % ls.length
    if (nBounded < 0) rotate(nBounded + ls.length, ls)
    else (ls drop nBounded) ::: (ls take nBounded)
  }

  
}