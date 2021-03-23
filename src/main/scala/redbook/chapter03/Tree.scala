package redbook.chapter03

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // 3.25
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right)
  }

  // 3.26
  def maximum(tree: Tree[Int]): Int = {
    def helper(tree: Tree[Int], currentMax: Int): Int = tree match {
      case Leaf(value) => value.max(currentMax)
      case Branch(left, right) => maximum(left).max(maximum(right))
    }

    helper(tree, Int.MinValue)
  }

  // 3.27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + depth(left).max(depth(right))
  }

  // 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  // 3.29
  def fold[A, B](tree: Tree[A])(f: A => B): Tree[B] = ???
}
