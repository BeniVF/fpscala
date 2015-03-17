package gettingstarted

import scala.annotation.tailrec

object MyModule {

  def fib(n: Int): Int = {
    @tailrec
    def go(i: Int, acc: Int, previous: Int): Int = {
      if (i == n)
        acc
      else
        go(i + 1, acc + previous, acc)
    }
    if (n == 0) n else go(1, 1, 0)
  }
}

object PolymorphicFunctions {

  // Here's a polymorphic version of `binarySearch`, parameterized on
  // a function for testing whether an `A` is greater than another `A`.
  def binarySearch[A](as: Array[A], key: A, gt: (A, A) => Boolean): Int = {
    @annotation.tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val a = as(mid2)
        val greater = gt(a, key)
        if (!greater && !gt(key, a)) mid2
        else if (greater) go(low, mid2, mid2 - 1)
        else go(mid2 + 1, mid2, high)
      }
    }
    go(0, 0, as.length - 1)
  }

  // Exercise 2: Implement a polymorphic function to check whether
  // an `Array[A]` is sorted
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(i: Int, as: Array[A]): Boolean = {
      if (as.isEmpty || as.size == 1)
        true
      else
      if (gt(as.head, as.tail.head))
        false
      else
        go(i + 1, as.tail)
    }
    go(0, as)
  }

  // Polymorphic functions are often so constrained by their type
  // that they only have one implementation! Here's an example:

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  // Exercise 3: Implement `curry`.

  // Note that `=>` associates to the right, so we could
  // write the return type as `A => B => C`
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => partial1(a, f)

  // NB: The `Function2` trait has a `curried` method already

  // Exercise 4: Implement `uncurry`
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  /*
  NB: There is a method on the `Function` object in the standard library,
  `Function.uncurried` that you can use for uncurrying.
  Note that we can go back and forth between the two forms. We can curry
  and uncurry and the two forms are in some sense "the same". In FP jargon,
  we say that they are _isomorphic_ ("iso" = same; "morphe" = shape, form),
  a term we inherit from category theory.
  */

  // Exercise 5: Implement `compose`

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}