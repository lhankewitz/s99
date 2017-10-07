package com.sepoe._99_2._1

object LastElementOfList {

  def main(args: Array[String]): Unit = {  
	  
	  val l = List(1,2,3,4,5,6,7)
//	  val l = List(1)
	  val solutions = Array[List[Int] => Int](
	 		  last1 _
	 		  , last2 _
	 		  , last3 _
	 		  , last4 _
	 		  , lastRecursive _
	 		  )

	 solutions.foreach(f => println(f(l)))		 
	  
  }
  
  def last1(l:List[Int]):Int = {
	  l.last
  }
  
  def last2(l:List[Int]):Int = {
	  l.reverse.head
  }
  
  def last3(l:List[Int]):Int = {
	  l(l.length -1)
  }
  
   def last4(l:List[Int]):Int = l match {
	   case Nil => error("Undefined")
	   case x :: Nil => x
	   case x :: rest => last4(rest)
   }
      
   
   // musterloesung
   def lastRecursive[A](ls: List[A]): A = ls match {
    case h :: Nil  => h
    case _ :: tail => lastRecursive(tail)
    case _         => throw new NoSuchElementException
  }

}