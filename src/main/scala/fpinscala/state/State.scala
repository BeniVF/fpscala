package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}





object RNG {
  // Nb - this was called SimpleRNG in the book text
  
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

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = map(int)(Math.abs)(rng)

  def double(rng: RNG): (Double, RNG) = map(int)(_.toDouble)(rng)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (generatedInt, rng2) = int(rng)
    val (generatedDouble, rng3) = double(rng2)
    ((generatedInt, generatedDouble), rng3)
  }
  
  def doubleInt(rng: RNG): ((Double, Int), RNG) = map(intDouble) { case (a, b) => (b, a) }(rng)

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (first, rng2) = double(rng)
    val (second, rng3) = double(rng2)
    val (third, rng4) = double(rng3)
    ((first, second, third), rng4)
  }


  def ints(count: Int)(rng: RNG): (List[Int], RNG) = (1 to count).foldLeft((List.empty[Int], rng)) { case (acc, _) =>
    val (result, rng) = acc
    val (nextInt, nextRng) = int(rng)
    (result :+ nextInt, nextRng)

  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }
}


case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, newS) = run(s)
    (f(a), newS)
  })

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = for {
    a <- this
    b <- sb
  } yield f(a, b)

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
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

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State { s =>
    inputs.foldLeft((0, 0), s) { case (((candies, coins), machine), input) => input match {
      case Coin if machine.candies > 0 => ((candies, coins + 1), machine.copy(locked = false, coins = coins + 1))
      case Coin if machine.candies == 0 => ((candies, coins), machine.copy(locked = true))
      case Turn if !machine.locked && machine.candies > 0 && machine.coins > 0 =>
        ((candies -1, coins), machine.copy(locked = true, coins = machine.coins, candies = machine.candies - 1))
      case _ => ((s.candies, s.coins), machine)
    }
    }
  }

}
