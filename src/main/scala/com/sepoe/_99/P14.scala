package com.sepoe._99_2

/**
Duplicate the elements of a list.
    Example:

    scala> duplicate(List('a, 'b, 'c, 'c, 'd))
    res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
 */
object P14 {

def main(args: Array[String]): Unit = {	
	  val list = List('a, 'b, 'c, 'c, 'd)
	  
	  
	  funcs[Symbol].foreach{ f => println(f(list)) }	  
  }
  
  def funcs[A] = Array[List[A] => List[A]](
		  duplicate _
		  , duplicate2 _
		  , duplicate3 _
		  , duplicate4 _
  		)

  def duplicate[A](list:List[A]):List[A] = 
	  list.flatMap{s => List(s,s)}
  
  def duplicate2[A](list:List[A]):List[A] = 
	for( s <- list; i <- 1 to 2) yield s  
	
   def duplicate3[A](list:List[A]):List[A] = 
	   if(list == Nil) Nil
	   else list.head :: list.head :: duplicate3(list.tail) 
	
   def duplicate4[A](list:List[A]):List[A] = list match{
	  	   case Nil => Nil
	  	   case x :: xs => x :: x :: duplicate4(xs) 
	   }	   
	
}