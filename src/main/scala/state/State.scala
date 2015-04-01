package state

import state.State


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  val doubleRand: Rand[Double] = map(int) {
    value =>
      value.abs.toDouble / Int.MaxValue.toDouble
  }

  def intsRand(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a =>
      unit(f(a))
    )

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) {
      a =>
        flatMap(rb) {
          b =>
            unit(f(a, b))
        }
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))


  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeIntRand: Rand[Int] = flatMap(int) {
    a => unit(if (a < 0) -(a + 1) else a)
  }

  def positiveMax(n: Int): Rand[Int] = map(int) {
    value =>
      value.abs % (n + 1)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (value: Int, newRng) = rng.nextInt
    (if (value < 0) -(value + 1) else value, newRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (value: Int, newRng) = nonNegativeInt(rng)
    (value.toDouble / Int.MaxValue.toDouble, newRng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (intValue: Int, newRng) = nonNegativeInt(rng)
    val (doubleValue: Double, lastRng) = double(newRng)
    ((intValue, doubleValue), lastRng)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((intValue, doubleValue), newRng) = intDouble(rng)
    ((doubleValue, intValue), newRng)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (firtValue, firstRng) = double(rng)
    val (secondValue: Double, secondRng) = double(firstRng)
    val (thirdValue: Double, thirdRng) = double(secondRng)
    ((firtValue, secondValue, thirdValue), thirdRng)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (1 to count).foldLeft((List[Int](), rng)) {
      case ((list, rng), _) =>
        val (value: Int, newRng) = rng.nextInt
        (list :+ value, newRng)
    }
  }
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State[S, B] { s =>
      val (a, newS) = run(s)
      (f(a), newS)
    }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    for {
      a <- this
      b <- sb
    } yield f(a, b)

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, newS) = run(s)
      f(a).run(newS)
    })
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}