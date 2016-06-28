package ch2

/**
  * Created by jmontroy on 6/19/16.
  */
object exercises extends App {

  // 2.1
  def signum(x: Int): Int = {
    if (x > 0) 1
    else if (x == 0) 0
    else -1
  }

  // 2.2
  val x = {}
  println(x,x.getClass)

  // 2.4
  for (i <- 0 to 10) println(10 - i)

  // 2.5
  def countdown(n: Int) { // note the slight syntax variation!
    for (i <- 0 to n) println(n - i)
  }




}
