package com.sepoe._99



/**
P23 (**) Extract a given number of randomly selected elements from a list.
    Example:

    scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
    res0: List[Symbol] = List('e, 'd, 'a)

    Hint: Use the solution to problem P20 
 */
object P23 {

def main(args: Array[String]): Unit = {	
	  val list = List('a, 'b, 'c, 'd, 'f, 'g, 'h)
	  
	 val n = 3
	  
	  funcs[Symbol].foreach{ f => println(f(n,list)) }  	   
	   
 }
  
  def funcs[A] = Array[(Int, List[A]) => List[A]](
		  randomSelect1a _
		  , randomSelect2a _

  		)

  		
  import P20.removeAt		
  		
  def randomSelect1a[A](k:Int, list:List[A]):List[A] = {
	  import scala.util.{Random => R}
	   if (k > 0 && list != Nil) {
		   val i = R.nextInt(list.length)
		   val (l, x) = removeAt(i, list)
		   x :: randomSelect1(k-1, l)
	  } else Nil
  }

	def randomSelect2a[A](k:Int, list:List[A]):List[A] = {
		import scala.util.Random.nextInt
		removeAt(nextInt(list.length), list) match {
			case (Nil,x) => x :: Nil
			case (l,x) if k > 0 => x :: randomSelect2a(k-1, l)
			case _ => Nil
		}
	}
	
	
	// sample solutions
	// ----------------------------
 def randomSelect1[A](n: Int, ls: List[A]): List[A] =
    if (n <= 0) Nil
    else {
      val (rest, e) = removeAt((new util.Random).nextInt(ls.length), ls)
      e :: randomSelect1(n - 1, rest)
    }

  // It can be expensive to create a new Random instance every time, so let's
  // only do it once.
  def randomSelect[A](n: Int, ls: List[A]): List[A] = {
    def randomSelectR(n: Int, ls: List[A], r: util.Random): List[A] =
      if (n <= 0) Nil
      else {
        val (rest, e) = removeAt(r.nextInt(ls.length), ls)
        e :: randomSelectR(n - 1, rest, r)
      }
    randomSelectR(n, ls, new util.Random)
  }
	
	
}