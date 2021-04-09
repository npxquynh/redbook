package redbook.chapter03

import org.specs2.mutable.Specification

class TreeSpec extends Specification {

  object Fixture {

    val defaultTree: Tree[String] = Branch(
      Branch(Leaf("alice"), Leaf("bob")),
      Branch(Leaf("charlotte"), Leaf("daniel"))
    )

    val defaultIntTree: Tree[Int] = Branch(
      Branch(Leaf(5), Leaf(10)),
      Branch(Leaf(1), Branch(Leaf(100), Leaf(23)))
    )
    val oneNodeTree: Tree[Int] = Leaf(1)
  }

  import Fixture._

  "Exercise 3.25 - size" in {
    Tree.size(defaultTree) ==== 4
  }

  "Exercise 3.26 - maximum" in {
    Tree.maximum(defaultIntTree) ==== 100
    Tree.maximum(oneNodeTree) ==== 1
  }

  "Exercise 3.27 - depth" in {
    Tree.depth(oneNodeTree) ==== 1
    Tree.depth(defaultTree) ==== 3
    Tree.depth(defaultIntTree) ==== 4
  }

  "Exercise 3.28 - map - string length" in {
    Tree.map(defaultTree)(_.size) ==== Branch(
      Branch(Leaf(5), Leaf(3)),
      Branch(Leaf(9), Leaf(6))
    )
  }

  "Exercise 3.29 - generalSizeMaxDepth" in {
    // size
    val leafSizeFn: Leaf[Int] => Int     = _ => 1
    val combineSizeFn: (Int, Int) => Int = _ + _
    val generalSizeFn                    = Tree.generalSizeMaxDepth(leafSizeFn, combineSizeFn) _
    generalSizeFn(defaultIntTree) ==== 5

    // maximum
    val leafMaximumFn: Leaf[Int] => Int     = _.value
    val combineMaximumFn: (Int, Int) => Int = _.max(_)
    val generalMaximumFn                    = Tree.generalSizeMaxDepth(leafMaximumFn, combineMaximumFn) _
    generalMaximumFn(defaultIntTree) ==== 100

    // depth
    val leafDepthFn: Leaf[Int] => Int     = _ => 1
    val combineDepthFn: (Int, Int) => Int = 1 + _.max(_)
    val generalDepthFn                    = Tree.generalSizeMaxDepth(leafDepthFn, combineDepthFn) _
    generalDepthFn(oneNodeTree) ==== 1
    generalDepthFn(defaultIntTree) ==== 4
  }

  "Exercise 3.29 - fold" in {
    // size
    val leafSizeFn: Leaf[Int] => Int     = _ => 1
    val combineSizeFn: (Int, Int) => Int = _ + _
    Tree.fold(defaultIntTree, leafSizeFn)(combineSizeFn) ==== 5

    // maximum
    val leafMaximumFn: Leaf[Int] => Int     = _.value
    val combineMaximumFn: (Int, Int) => Int = _.max(_)
    Tree.fold(defaultIntTree, leafMaximumFn)(combineMaximumFn) ==== 100

    // depth
    Tree.generalDepth(oneNodeTree) ==== Tree.depth(oneNodeTree)
    Tree.generalDepth(defaultIntTree) ==== Tree.depth(defaultIntTree)

    // map
    val anotherTree: Tree[String] = Branch(
      Branch(Leaf("alice"), Branch(Leaf("bob"), Leaf("charlotte"))),
      Branch(Leaf("daniel"), Leaf("emma"))
    )
    Tree.generalMap(defaultTree)(_.size) ==== Tree.map(defaultTree)(_.size)
    Tree.generalMap(anotherTree)(_.size) ==== Tree.map(anotherTree)(_.size)
  }
}
