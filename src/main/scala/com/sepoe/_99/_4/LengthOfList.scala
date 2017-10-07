package com.sepoe._99._4

/**
 * The Class LengthOfList.
 * 
 * http://aperiodic.net/phil/scala/s-99/
 * 
 */
object LengthOfList {

  def main(args: Array[String]): Unit = {  
	  
	val l = List(1,2,3,4,5,6,7,8)
	
	funcs[Int].foreach( f => println( "length of " + l + " = " + f(l)))
  }
  
  
  def funcs[A] =  Array[List[A] => Int](
	length1 _	
	, length2 _
	, length3 _
	, length4 _
	, length5 _
	, length6 _
	, length7 _		
	, length8 _
	, length9 _
	, length10 _
	, length11 _
			
	)  
  
  def length1[A](list:List[A]):Int = list.foldLeft(0){ (sum, _) => sum + 1}
  
  def length2[A](list:List[A]):Int = list.length
  
  def length3[A](list:List[A]):Int = {
	  val (items, indices):(List[A], List[Int]) = list.zipWithIndex unzip
	  
	  (indices max) + 1
  }
  
  def length4[A](list:List[A]):Int = {
	  (list.zipWithIndex unzip)._2.max + 1 
  }
   
  def length5[A](list:List[A]):Int =  list match {
	  case Nil => 0
	  case _ => 1 + length5(list.tail)
  }
  
  def length6[A](list:List[A]):Int =  list match {
	  case Nil => 0
	  case x :: xs => 1 + length5(xs)
  }
  
  def length7[A](list:List[A]):Int = (0 /: list) { (sum,_) => sum + 1 }
  
  def length8[A](list:List[A]):Int = (list :\ 0) { (_, sum) => sum + 1 }
  
  
  /**
   * Tail recursive. 
   * 
   * Length9.
   *
   * @param list the list
   * @return the int
   */
  def length9[A](list:List[A]):Int = {
	  def localLength[A](sum:Int, list:List[A]):Int  = list match {
	 	  case Nil => sum 
	 	  case x :: xs => localLength(sum + 1, xs)
	  }
	  
	   localLength(0, list)
  }
  
  
    def length10[A](list:List[A]):Int = {
    	var sum = 0
    	for( x <- list){sum += 1}
    	
    	sum
    }
    
  def length11[A](list:List[A]):Int = {
		  var sum = 0
		  var localList = list
		  
		  while(localList != Nil){
			  sum += 1
			  localList = localList.tail
		  }
			  		  
		  sum
  }

  
  

}