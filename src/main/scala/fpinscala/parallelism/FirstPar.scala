package fpinscala.parallelism

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.Future
import java.util.concurrent.Callable

object FirstPar {

  type Par[A] = ExecutorService => A

  def unit[A](a: A): Par[A] = ec => a

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = unit(f(run(a), run(b)))

  def fork[A](a: => Par[A]): Par[A] = { ec =>
    val future = ec.submit { new Callable[A] {
        def call(): A = run(a)
      }
    }
    future.get
  }

  def run[A](a: Par[A]): A = a(Executors.newFixedThreadPool(5))
}

