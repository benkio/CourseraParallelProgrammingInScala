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
    var balanceCount = 0
    for (c <- chars){
      if (c == '(') balanceCount += 1
      if (c == ')') balanceCount -= 1
      if (balanceCount < 0) return false
    }
    balanceCount == 0

    /*val countPars =
      chars
        .filter(x => x == '(' || x == ')')
        .map(x => x match {
                case '(' => 1
                case ')' => -1
              })
      .scanLeft(0)(_ + _)
      .toList
//    println(chars.mkString("")  + " " + countPars)
    countPars.reverse.head == 0 && countPars.forall(_ >= 0)

     */
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int) : (Int,Int) = {
      var minBalance, balanceCount = 0
      for (i <- idx until until) {
        val c = chars(i)
        if (c == '(') balanceCount += 1
        if (c == ')') balanceCount -= 1
        if (balanceCount < minBalance) minBalance = balanceCount
      }
      (minBalance, balanceCount)
      /*
       val charsPortion = for (i <- idx until until) yield chars(i)
      val charsElab =
        charsPortion
          .filter(x => x == '(' || x == ')')
          .map(x => x match {
                 case '(' => 1
                 case ')' => -1
               })
          .scanLeft(0)(_ + _).toList
      (charsElab.min, charsElab.reverse.head)
       */
    }

    def reduce(from: Int, until: Int) : (Int, Int)  = {
      if (until - from <= threshold) traverse(from, until)
      else {
        val mid = from + (until - from) / 2
        val (left, right) = parallel(reduce(from, mid), reduce(mid, until))
        (Math.min(left._1, left._2 + right._1), (left._2 + right._2))
      }
    }
    // println(reduce(0, chars.length) + " - "  + chars.mkString)
    reduce(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
