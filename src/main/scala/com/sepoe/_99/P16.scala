package com.sepoe._99

/**
P16 (**) Drop every Nth element from a list.
    Example:

    scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
 */
object P16 {

  def main(args: Array[String]): Unit = {	
	  val list = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
	  
	  
	  funcs[Symbol].foreach{ f => println(f(3,list)) }	  
  }
  
  def funcs[A] = Array[(Int,List[A]) => List[A]](
		  drop1 _
		  ,drop2 _
		  ,drop3 _
		  ,drop4 _
		  ,drop5 _
		  , dropRecursive _
		  , dropTailRecursive _
  		)

  	def drop1[A](n:Int, list:List[A]):List[A] = 
  		(for(i<- 0 until list.length if (i+1)%n != 0) yield list(i)).toList
  		
  	def drop2[A](n:Int, list:List[A]):List[A] = 
  		for( (x, i)<-list zipWithIndex; if (i+1)%n != 0) yield x
  		
	def drop3[A](n:Int, list:List[A]):List[A] = {
  		def inc(i:Int, list:List[A]) : List[A] =
  			if(list == Nil) Nil
  			else if(i%3 == 0) inc(i+1, list.tail)
  			else list.head :: inc(i+1, list.tail)
  		
  		inc(1, list)
  	} 
  
	def drop4[A](n:Int, list:List[A]):List[A] = 
  		list.zipWithIndex filter{t => (t._2 + 1)%3 != 0} map {_._1}

  	def drop5[A](n:Int, list:List[A]):List[A] = 
  		list.foldLeft((1, List[A]())){
  			case ((i,l), x) => if(i%n != 0) (i+1, x::l) else (i+1, l)
  		}._2
	
  		
  // Simple recursion.
  def dropRecursive[A](n: Int, ls: List[A]): List[A] = {
    def dropR(c: Int, curList: List[A]): List[A] = (c, curList) match {
      case (_, Nil)       => Nil
      case (1, _ :: tail) => dropR(n, tail)
      case (_, h :: tail) => h :: dropR(c - 1, tail)
    }
    dropR(n, ls)
  }

  // Tail recursive.
  def dropTailRecursive[A](n: Int, ls: List[A]): List[A] = {
    def dropR(c: Int, curList: List[A], result: List[A]): List[A] = (c, curList) match {
      case (_, Nil)       => result.reverse
      case (1, _ :: tail) => dropR(n, tail, result)
      case (_, h :: tail) => dropR(c - 1, tail, h :: result)
    }
    dropR(n, ls, Nil)
  }
  		
	
}