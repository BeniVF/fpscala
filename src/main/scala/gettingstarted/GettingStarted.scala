package gettingstarted

import scala.annotation.tailrec

object MyModule {

  def fib(n: Int): Int = {
    @tailrec
    def go(i: Int, n: Int, acc: Int, previous: Int): Int = {
      if (i == n)
        acc
      else
        go(i + 1, n, acc + previous, acc)
    }
    if (n == 0) n else go(1, n, 1, 0)
  }
}