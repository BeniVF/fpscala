package fpinscala.monoid


trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}


object Monoid {

  val stringAddition: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    def zero = ""
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2

    def zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2

    def zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2

    def zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2

    def zero = true
  }

  def option[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    def zero: Option[A] = None
  }

  def endo[A]: Monoid[A => A] = new Monoid[(A) => A] {
    def op(a1: (A) => A, a2: (A) => A) = a1 compose a2

    def zero = identity
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.map(f).foldLeft(m.zero)(m.op)

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v match {
    case a if a.isEmpty => m.zero
    case a if a.size == 1 => f(v.head)
    case _ =>
      val (l, r) = v.splitAt(v.size / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
  }
}