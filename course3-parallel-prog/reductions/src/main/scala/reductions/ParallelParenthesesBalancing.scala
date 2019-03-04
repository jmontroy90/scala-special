package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def pop(chars: Array[Char], left: Int, right: Int): Boolean = {
      if (chars.isEmpty) {
        if (left != right) false // if not balanced lefts and rights
        else true
      } else if (right > left) {
        false // if your right parens get ahead, false
      } else {
        if (chars.head == '(') pop(chars.tail, left + 1, right) // left
        else if (chars.head == ')') pop(chars.tail, left, right + 1) // right
        else pop(chars.tail, left, right) // other chars
      }
    }

    pop(chars, 0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int): (Int, Int) = {
      var (_idx, _left, _right) = (idx, 0, 0)
      while (_idx < until) {
        if (chars(_idx) == '(') _left += 1
        else if (chars(_idx) == ')') _right += 1
        else { } // do nothing
        _idx += 1
      }
      (_left, _right)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until)
      } else {
        val mid = from + (until - from) / 2
        val t = parallel(
          reduce(from, mid),
          reduce(mid, until)
        )
        (t._1._1 + t._2._1, t._1._2 + t._2._2)
      }

    }

    val res = reduce(0, chars.length)
    if (res._1 < 0) false
    else if (res._1 == res._2) true
    else false
  }
}
