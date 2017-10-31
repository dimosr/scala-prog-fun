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
    var currentParSum = 0
    var index = 0

    while(index < chars.length) {
      if(currentParSum < 0) return false

      if(chars(index) == '(') currentParSum += 1
      else if(chars(index) == ')') currentParSum -= 1

      index += 1
    }

    currentParSum == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int) : (Int, Int) = {
      var rightOpen = 0
      var currentSum = 0
      var index = idx

      while(index < until) {
        if (chars(index) == '(') {
          currentSum += 1
        } else if(chars(index) == ')') {
          if(currentSum == 0)
            rightOpen += 1
          else
            currentSum -= 1
        }

        index += 1
      }

      (currentSum, rightOpen)
    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      val actualThreshold = Math.max(1, threshold)

      if(until <= from)
        (0, 0)
      else {
        if((until - from) > actualThreshold) {
          val mid = from + (until - from) / 2
          val ((left1, right1), (left2, right2)) = parallel({reduce(from, mid)}, {reduce(mid, until)})

          if(left1 > right2)
            (left2 + (left1 - right2), right1)
          else
            (left2, right1 + (right2 - left1))
        } else
          traverse(from, until)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

}
