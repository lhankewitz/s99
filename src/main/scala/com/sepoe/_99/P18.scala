package com.sepoe._99

/**
 *
P18 (**) Extract a slice from a list.
    Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.

    Example:

    scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    res0: List[Symbol] = List('d, 'e, 'f, 'g)
 */
object P18 {

 def main(args: Array[String]): Unit = {	
	  val list = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
	  
	 val i = 3
	 val k = 7
	  
	  funcs[Symbol].foreach{ f => println(f(i,k,list)) }	  
	   
  }
  
  def funcs[A] = Array[(Int, Int,List[A]) => List[A]](
		  slice1 _
		  , slice2 _
		  , slice3 _
  		)

  		
  def slice1[A](i:Int, k:Int, list:List[A]):List[A] = {
	  require(k >= i, "k must not be less than i")
	  list.drop(i).take(k-i)
  }
  
 def slice2[A](i:Int, k:Int, list:List[A]):List[A] = 
	 (list.zipWithIndex).filter{t => {t._2 >= i && t._2 < k}}.unzip _1	 
  
  def slice3[A](i:Int, k:Int, list:List[A]):List[A] = list.slice(i, k)
  

}