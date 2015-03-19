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
    def maxValue(first: Option[Int], second: Option[Int]): Option[Int] = (first, second) match {
      case (Some(firstValue), Some(secondValue)) if firstValue >= secondValue => first
      case (Some(firstValue), Some(secondValue)) if secondValue > firstValue => second
      case (Some(_), None) => first
      case (None, Some(_)) => second
    }
    def go(tree: Tree[Int], max: Option[Int] ): Option[Int] = (tree, max) match {
      case (Leaf(value), None) => Some(value)
      case (Leaf(value), Some(maxValue)) if value>maxValue  => Some(value)
      case (Branch(left, right), maxOption) => {
        val maxLeft = maximum(left)
        val maxRight = maximum(right)
        maxValue(maxValue(maxLeft, maxRight), max)
      }
      case _ => max
    }
    go(tree, None)
  }

}