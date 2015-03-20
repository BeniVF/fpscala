package datastructures


sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(tree: Tree[Int]): Int = fold(tree)(identity)(_ max _ )


  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => {
      val leftDepth = depth(left)
      val rightDepth = depth(right)
      val currentDepth = if (leftDepth > rightDepth) leftDepth else rightDepth
      currentDepth + 1
    }
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }


  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B,B) => B): B = tree match {
    case Leaf(value) => f(value)
    case Branch(left, right) => {
      g(fold(left)(f)(g), fold(right)(f)(g))
    }
  }

}