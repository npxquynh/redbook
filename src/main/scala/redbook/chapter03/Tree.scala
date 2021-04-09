package redbook.chapter03

sealed trait Tree[+A]
case class Leaf[+A](value: A)                        extends Tree[A]
case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // 3.25
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_)             => 1
    case Branch(left, right) => size(left) + size(right)
  }

  // 3.26
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(value)         => value
    case Branch(left, right) => maximum(left).max(maximum(right))
  }

  // 3.27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_)             => 1
    case Branch(left, right) => 1 + depth(left).max(depth(right))
  }

  // 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value)         => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  // 3.29
  def generalSizeMaxDepth(leafFn: Leaf[Int] => Int, combineFn: (Int, Int) => Int)(tree: Tree[Int]): Int = tree match {
    case l @ Leaf(_) => leafFn(l)
    case Branch(left, right) =>
      combineFn(generalSizeMaxDepth(leafFn, combineFn)(left), generalSizeMaxDepth(leafFn, combineFn)(right))
  }

  def fold[A, B](tree: Tree[A], leafFn: Leaf[A] => B)(f: (B, B) => B): B = tree match {
    case l @ Leaf(_) => leafFn(l)
    case Branch(left, right) =>
      f(
        fold(left, leafFn)(f),
        fold(right, leafFn)(f)
      )
  }

  def generalDepth[A](tree: Tree[A]): Int = {
    val leafDepthFn: Leaf[A] => Int     = _ => 1
    val combineDepthFn: (Int, Int) => Int = 1 + _.max(_)

    fold(tree, leafDepthFn)(combineDepthFn)
  }

  def generalMap[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    def leafFn(leaf: Leaf[A]): Tree[B] = Leaf(f(leaf.value))
    def combineFn(left: Tree[B], right: Tree[B]): Tree[B] = Branch(left, right)

    fold(tree, leafFn)(combineFn)
  }
}
