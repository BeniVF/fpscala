package fpinscala.parallelism

import java.util.concurrent._

import scala.language.implicitConversions

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = _ => UnitFuture(a)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = { es =>
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(())) { (a, _) => f(a) }

  def fork[A](a: => Par[A]): Par[A] = { es =>
    es.submit {
      new Callable[A] {
        def call(): A = a(es).get
      }
    }
  }

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = es =>
    if (cond(es).get)
      t(es)
    else
      f(es)


  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)


  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sequence_simple[A](l: List[Par[A]]): Par[List[A]] =
    l.foldLeft(unit(List.empty[A])) { (acc, current) =>
      map2(acc, current) { case (list, value) => list :+ value }
    }

  def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
    as match {
      case Nil => unit(List.empty[A])
      case x :: xs => map2(x, fork(sequenceRight(xs))) {
        _ :: _
      }
    }

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    as match {
      case xs if xs.isEmpty => unit(Vector.empty[A])
      case xs if xs.length ==1 => map(xs.head)(Vector(_))
      case xs =>
        val (l,r) = xs.splitAt(xs.length/2)
        map2(sequenceBalanced(l), sequenceBalanced(r)){_ ++ _}
    }
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] = map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val parList = l.map(asyncF(a => if (f(a)) List(a) else List()))
    map(sequence(parList))(_.flatten)
  }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = map2(n, sequence(choices)) { (i, list) =>
    list.apply(i)
  }

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = es => {
    val k = run(es)(key).get
    run(es)(choices(k))
  }

  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {

  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get


  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

}

