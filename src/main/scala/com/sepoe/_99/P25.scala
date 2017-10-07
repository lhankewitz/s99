package com.sepoe._99

/**
 *
P25 (*) Generate a random permutation of the elements of a list.
    Hint: Use the solution of problem P23.

    Example:

    scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
    res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
 */
object P25 {

def main(args: Array[String]): Unit = {	
	  val list = List('a, 'b, 'c, 'd, 'e, 'f)
	  
	  
	  funcs[Symbol].foreach{ f => println(f(list)) }  	   
	   
 }
  
  def funcs[A] = Array[List[A] => List[A]](
		  randomPermute1 _
		  , randomPermute2 _
		  , randomPermute3 _
  		)
  		
  import P24.lotto1		
  import P23.randomSelect

  def randomPermute1[A](l:List[A]):List[A] = 
	  for(i <- lotto1(l.length, l.length)) yield l(i-1)

  def randomPermute2[A](l:List[A]):List[A] = 
	  lotto1(l.length, l.length).map{i => l(i-1) }

  def randomPermute3[A](l:List[A]):List[A] = randomSelect(l.length, l)
  
 
  
  /* Fisher yates
    for i from n - 1 downto 1 do
       j <- random integer with 0 <= j <= i
       exchange a[j] and a[i]
   * */
  def randomPermute4[A](l:List[A]):List[A] = {
	  import scala.collection.mutable._
	  import scala.util.Random.nextInt
	  val all = new ArrayBuffer[A] ++ l
	  for(i <- (l.length-1) to 1 by -1; val j = nextInt(i+1)) {
	 	  val tmp = all(j)
	 	  all(j) = all(i)
	 	  all(i) = tmp
	  }
	  all.toList 
  } 
  
  // Efficient purely functional algorithms for shuffling are a lot harder.  One
  // is described in http://okmij.org/ftp/Haskell/perfect-shuffle.txt using
  // Haskell. Implementing it in Scala is left as an exercise for the reader.
  // TODO
}













