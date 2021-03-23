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
    Tree.depth(oneNodeTree) ==== 0
    Tree.depth(defaultTree) ==== 2
    Tree.depth(defaultIntTree) ==== 3
  }

  "Exercise 3.28 - map" in {
    Tree.map(defaultTree)(_.size) ==== Branch(
      Branch(Leaf(5), Leaf(3)),
      Branch(Leaf(9), Leaf(6))
    )
  }

  "Exercise 3.29 - fold" in {
    ok
  }
}
