package laziness

import Stream._
import errorhandling._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), 1) => Some((h(), (empty, 0)))
    case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
    case _ => None
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => t().drop(n - 1)
    case Cons(h, t) if n == 1 => t()
    case _ => Empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true) {
    case (current, result) => result && p(current)
  }

  def headOption: Option[A] = this match {
    case Cons(h, t) => Some(h())
    case _ => None
  }

  def map[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B]) {
    case (current, result) =>
      f(current) append result
  }

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A]) {
    case (current, result) =>
      if (f(current)) cons(current, result) else result
  }

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((current, result) => cons(current, result))

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) =>
      Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }

  def zip[B](s2: Stream[B]): Stream[(A, B)] = this.zipWith(s2)((_, _))


  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) =>
      Some((f(Some(h1()), Some(h2())), (t1(), t2())))
    case (Cons(h1, t1), Empty) =>
      Some((f(Some(h1()), None), (t1(), empty[B])))
    case (Empty, Cons(h2, t2)) =>
      Some((f(None, Some(h2())), (empty[A], t2())))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    this.zipWithAll(s2)((_, _))

  def startsWith[B](s: Stream[B]): Boolean =
    this.zipAll(s).takeWhile(_._2 != None) forAll {
      case (h, h2) => h == h2
    }

  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case s => Some((s, s drop 1))
  } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean = this.tails.exists(_ startsWith s)

  def toList: List[A] = foldRight(List[A]())((current, result) => current +: result)
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def toList[A](a: Stream[Stream[A]]): List[List[A]] =
    a.toList.map(_.toList)

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = constant(1)

  def constant[A](a: A): Stream[A] = unfold(a)(x => Some((x, x)))

  def from(n: Int): Stream[Int] = unfold(n)(x => Some((x, x + 1)))

  val fibs: Stream[Int] = unfold((0, 1)) {
    case (first, second) => Some(first, (second, first + second))
    case _ => None
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def go(current: S): Stream[A] = {
      f(current) match {
        case None => empty[A]
        case Some((value, next)) => cons(value, go(next))
      }
    }
    go(z)
  }
}