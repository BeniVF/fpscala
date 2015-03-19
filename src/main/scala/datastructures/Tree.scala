package datastructures


sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(tree: Tree[Int]): Option[Int] = {
    def max(first: Option[Int], second: Option[Int]): Option[Int] = (first, second) match {
      case (Some(firstValue), Some(secondValue)) if firstValue >= secondValue => first
      case (Some(firstValue), Some(secondValue)) if secondValue > firstValue => second
      case (Some(_), None) => first
      case (None, Some(_)) => second
    }
    def go(tree: Tree[Int], currentMax: Option[Int]): Option[Int] = (tree, currentMax) match {
      case (Leaf(value), None) => Some(value)
      case (Leaf(value), Some(maxValue)) if value > maxValue => Some(value)
      case (Branch(left, right), maxOption) => {
        val maxLeft = maximum(left)
        val maxRight = maximum(right)
        max(max(maxLeft, maxRight), currentMax)
      }
      case _ => currentMax
    }
    go(tree, None)
  }


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

}