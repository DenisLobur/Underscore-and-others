package rockjvm

import scala.annotation.tailrec

object Recursion extends App {
  /**
   * 1. Concatenate strings n times
   * 2. isPrime function is trail recursive
   * 3. Fibonacci function is tail recursive
   */

  def stringNTimes(str: String, n: Int): String = {
    if (n <= 1) str
    else str + stringNTimes(str, n - 1)
  }

  println(stringNTimes("hello", 5))

  @tailrec
  def stringNTimesTailrec(str: String, n: Int, accumulator: String): String = {
    if (n <= 0) accumulator
    else stringNTimesTailrec(str, n - 1, str + accumulator)
  }

  println(stringNTimesTailrec("hello", 5, ""))

  def isPrime(n: Int): Boolean = {
    def divisible(t: Int): Boolean = {
      if (t <= 1) true
      else n % t != 0 && divisible(t - 1)
    }

    divisible(n / 2)
  }

  println("isPrivate: " + isPrime(17))

  def isPrimeTailrec(n: Int): Boolean = {
    def divisible(t: Int, isStillPrime: Boolean): Boolean = {
      if (!isStillPrime) false
      else if (t <= 1) true
      else divisible(t - 1, n % t != 0 && isStillPrime)
    }

    divisible(n / 2, true)
  }

  println("isPrivateTailRec: " + isPrimeTailrec(13))

  def fibonacci(n: Int): Int = {
    if (n <= 2) 1
    else fibonacci(n - 1) + fibonacci(n - 2)
  }

  // 1, 1, 2, 3, 5, 8, 13, 21, 34
  println(fibonacci(8))

  def fibonacciTailrec(n: Int): Int = {
    def sumUp(i: Int, last: Int, nextToLast: Int): Int = {
      if (i >= n) last
      else sumUp(i + 1, last + nextToLast, last)
    }

    if (n <= 2) 1
    else sumUp(3, 1, 1)
  }

  println(fibonacciTailrec(8))
}
