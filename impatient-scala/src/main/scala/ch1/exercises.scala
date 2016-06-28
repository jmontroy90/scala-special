package ch1
import scala.math._
import scala.util.Random
import scala.BigInt._

/**
  * Created by jmontroy on 6/19/16.
  */
object exercises extends App {

  // 1.2 - Compute the square root of 3 and then square it. How much does it differ from 3?
  val x: Double = {3 - (sqrt(3)*sqrt(3))}
  println(x)

  // 1.4 - overriden operators
  println("crazy"*3)

  // 1.5 - syntax-lite
  println(10 max 2)

  // 1.7
  println(probablePrime(100, Random))

  // 1.9
  print("hello"(0)); println("hello"("hello".length-1))

  // 1.10
  println("hello".take(2))
  println("hello".drop(2))
  println("hello".takeRight(2))
  println("hello".dropRight(2))



}
