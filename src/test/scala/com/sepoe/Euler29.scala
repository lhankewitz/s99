package com.sepoe

// I just accumulated old code, can't remember where this is comming from. :-(
object Euler29 {

  def main(args: Array[String]): Unit = {  

   val result = scala.collection.mutable.Set[BigInt]()
   
   for(a <- 2 to 100; b<- 2 to 100) { result += BigInt(a).pow(b)}
   
    println(result.size )
    
    
    println((for(a <- 2 to 100; b<- 2 to 100) yield BigInt(a).pow(b)).distinct.size)
  }

}