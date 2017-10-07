package com.sepoe._99

/**
 *
P24 (*) Lotto: Draw N different random numbers from the set 1..M.
    Example:

    scala> lotto(6, 49)
    res0: List[Int] = List(23, 1, 17, 33, 21, 37)
 */
object P24 {

def main(args: Array[String]): Unit = {	
	  
	  funcs[Symbol].foreach{ f => println(f(6,49)) }  	   
	   
	  println (List.range(0,0)) 
 }
  
  def funcs[A] = Array[(Int, Int) => List[Int]](
		  lotto1 _
		  ,lotto2 _
  		)

  		
  import P23.randomSelect	
  		
  // i implemented the sample solution
  def lotto1(n:Int, m:Int):List[Int] = randomSelect(n, List.range(1, m + 1))

  def lotto2(n:Int, m:Int):List[Int] = {
	  import scala.collection.mutable._
	  var drawn = new ListBuffer[Int]()
	  var all = List.range(1,m)
	  for(i<- 1 to 6 ) { 
	 	val drawnNumber = randomSelect(1, all)(0)
	 	drawn += drawnNumber
	 	all = all.filter{ _ != drawnNumber }
	  }
	   
	  drawn.toList 
  }
}