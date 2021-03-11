package redbook.chapter03

import org.specs2.mutable.Specification

class ListSpec extends Specification {

  object Fixture {
    val emptyList      = List()
    val oneElementList = List(1)
    val defaultList    = List(1, 2, 3, 4, 5)
    val strList = List("apple", "baby", "Cat", "Dog")
  }

  import Fixture._

  "Exercise 3.1" in {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t)                            => h + List.sum(t)
      case _                                     => 101
    }

    x ==== 3
  }

  "Exercise 3.2 - tail" >> {
    "for empty list" in {
      List.tail(emptyList) ==== emptyList
    }

    "for list with 1 element" in {
      List.tail(oneElementList) ==== Nil
    }

    "for list with multiple elements" in {
      List.tail(defaultList) ==== List(2, 3, 4, 5)
    }
  }

  "Exercise 3.3 - setHead" >> {
    "for empty list" in {
      List.setHead(emptyList, 8) ==== emptyList
    }

    "for list with 1 element" in {
      List.setHead(oneElementList, 8) ==== List(8)
    }

    "for list with multiple elements" in {
      List.setHead(defaultList, 8) ==== List(8, 2, 3, 4, 5)
    }
  }

  "Exercise 3.4 - drop" in {
    List.drop(emptyList, 10) ==== emptyList
    List.drop(oneElementList, 1) ==== emptyList
    List.drop(defaultList, 1) ==== List(2, 3, 4, 5)
    List.drop(defaultList, 2) ==== List(3, 4, 5)
    List.drop(defaultList, 4) ==== List(5)
  }

  "Exercise 3.5 - dropWhile" in {
    val isEven: Int => Boolean = (x) => x % 2 == 0
    List.dropWhile(defaultList, isEven) ==== List(1, 3, 5)
    List.dropWhile(defaultList, (x: Int) => false) ==== defaultList
    List.dropWhile(defaultList, (x: Int) => true) ==== emptyList
  }

  "Exercise 3.5 - dropWhileCurried" in {
    val isEven: Int => Boolean = (x) => x % 2 == 0
    List.dropWhileCurried(defaultList)(isEven) ==== List(1, 3, 5)
    List.dropWhileCurried(defaultList)(x => false) ==== defaultList
    List.dropWhileCurried(defaultList)(_ => true) ==== emptyList
  }

  "Exercise 3.6 - init" in {
    List.init(emptyList) ==== emptyList
    List.init(oneElementList) ==== emptyList
    List.init(defaultList) ==== List(1, 2, 3, 4)
  }

  "Exercise 3.7 - early termination is not possible" in {
    /*
    No, it's not possible to immediately halt the recursion and return 0.0 if encounters a 0.0 for `product`. This is because in order to evaluate f, we need to call foldRight, and we need to foldRight until the end of the list
    */
    ok
  }

  "Excercise 3.8 - to recreate a list" in {
    /*
    foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
    Then we will have the same list back
    */

    List.foldRight(defaultList, Nil:List[Int])(Cons(_,_)) ==== defaultList
  }

  "Exercise 3.9 - length" in {
    List.length(emptyList) ==== 0
    List.length(oneElementList) ==== 1
    List.length(defaultList) ==== 5
  }

  "Exercise 3.10 - tail-recursive foldLeft" in {
    /*
    Why foldRight is not tail-recursive

    We cannot store the evaluation of f and the rest of the list in advance. And we cannot annotate
    @tailrec to foldRight
    */

    List.foldLeft(defaultList, 0)(_ + _) ==== 15
    List.foldLeft(defaultList, 1000.0)(_ / _) ==== 8
    List.foldRight(defaultList, 1000.0)(_ / _) ==== 8
  }

  "Exercise 3.11 - lengthWithFoldLeft" in {
    List.lengthWithFoldLeft(defaultList) ==== List.length(defaultList)
  }

  "Exercise 3.12 - reverse" in {
    List.reverse(defaultList) ==== List(5, 4, 3, 2, 1)
  }

  "Exercise 3.13 - write foldLeft in terms of foldRight" in {
    // foldLeft and foldRight are the same for associative + commutative function
    List.foldLeftInTermsOfFoldRight(defaultList, 0)(_ + _) ==== List.foldLeft(defaultList, 0)(_ + _)

    // string concatenation is associative, but not commutative
    // Hence string concatenation is a good method to test
    List.foldLeftInTermsOfFoldRight(strList, "")(_ + _) ==== List.foldLeft(strList, "")(_ + _)
    ok
  }

  "Exercise 3.13 - write foldRight in terms of foldLeft" in {
    ok
  }
}
