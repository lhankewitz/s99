package com.sepoe._99_2._5

/**
 * The Class ReverseList.
 * 
 */
object ReverseList {

  def main(args: Array[String]): Unit = {  
	  
	val l = List(1,2,3,4,5,6,7,8)
	
	funcs[Int].foreach( f => println( "reverse of " + l + " = " + f(l)))
  }
  
  
  def funcs[A] =  Array[List[A] => List[A]](
		reverse1 _	
		, reverse2 _	
		, reverse3 _	
		, reverse4 _	
		
	) 
	
	def reverse1[A](list:List[A]):List[A] = list.reverse
		
	def reverse2[A](list:List[A]):List[A] = list match {
	  case Nil => Nil
	  case x :: xs => reverse2(xs) :+ x 
    }

	def reverse3[A](list:List[A]):List[A] = {
		var reversed:List[A] = Nil
		
		for(x <- list){ reversed = x :: reversed }
		
		reversed
	}
	
	def reverse4[A](list:List[A]):List[A] = {
		import scala.collection.mutable.ListBuffer
			var reversed:ListBuffer[A] = ListBuffer()
			
//			for(x <- list){ x +: reversed }
			for(x <- list){ reversed.:+(x); println(reversed) }
	
		reversed.toList
	}
	
	
    
	
}