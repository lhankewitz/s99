// TODO implement the test via scala test
//package com.sepoe._99
//
//import org.specs2.mutable._
//
//import P09._
//
//class P09Spec extends Specification {
//
//  "The pack method " should {
//    "return List() for pack(List())" in {
//      pack(List()) == List(Nil) must beTrue
//    }
//    "return List(List('a)) for pack(List('a))" in {
//      pack(List('a)) == List(List('a)) must beTrue
//    }
//    "return List(List('a, 'a, 'a, 'a), List('b)) for pack(List('a, 'a, 'a, 'a, 'b))" in {
//      pack(List('a, 'a, 'a, 'a, 'b)) ==
//        List(List('a, 'a, 'a, 'a), List('b)) must beTrue
//    }
//    "return List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a)) for  pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a))" in {
//      pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a)) ==
//        List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a)) must beTrue
//    }
//  }
//}

