package fpinscala.monoid


trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}


object Monoid {
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2

    def zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2

    def zero = 1
  }

  val booleanOr: Monoid[Boolean] =  new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2

    def zero = false
  }

  val booleanAnd: Monoid[Boolean] =  new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2

    def zero = true
  }


  def option[A]: Monoid[Option[A]] =  new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    def zero : Option[A] = None
  }

  def endo[A]: Monoid[A => A] = new Monoid[(A) => A] {
    def op(a1: (A) => A, a2: (A) => A) = a1 compose a2

    def zero = identity
  }


}