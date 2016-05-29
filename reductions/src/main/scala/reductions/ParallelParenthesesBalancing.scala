package reductions

import scala.annotation._
import scala.math

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
    var count = 0
    var idx = 0
    var bottom = 0
    while (idx < chars.length) {
      val c = chars(idx)
      if (c == '(') {
        count += 1
      } else if (c == ')') {
        count -= 1
      }
      idx += 1
      bottom = bottom.min(count)
    }
    (count, bottom) == (0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, acc: Int, bottom: Int): (Int, Int) = {
      var cur = idx
      var newAcc = acc
      var newBottom = bottom
      while (cur < until) {
        if (chars(cur) == '(') {
          newAcc += 1
        } else if (chars(cur) == ')') {
          newAcc -= 1
          newBottom = newBottom.min(newAcc)
        }
        cur += 1
      }
      (newAcc, newBottom)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid = (from + until) / 2
        val (left, right) = parallel(reduce(from, mid), reduce(mid, until))

        (left._1+right._1, left._2.min(left._1+right._2))
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
