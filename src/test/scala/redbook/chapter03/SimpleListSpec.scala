package redbook.chapter03

import org.specs2.mutable.Specification

class SimpleListSpec extends Specification {

  object Fixture {
    val emptyList      = SimpleList()
    val oneElementList = SimpleList(1)
    val defaultList    = SimpleList(1, 2, 3, 4, 5)
    val additionalList = SimpleList(10, 11, 12)
    val strList        = SimpleList("apple", "baby", "Cat", "Dog")

    val isEvenFn: Int => Boolean = _ % 2 == 0
  }

  import Fixture._

  "Exercise 3.1" in {
    val x = SimpleList(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t)                            => h + SimpleList.sum(t)
      case _                                     => 101
    }

    x ==== 3
  }

  "Exercise 3.2 - tail" >> {
    "for empty list" in {
      SimpleList.tail(emptyList) ==== emptyList
    }

    "for list with 1 element" in {
      SimpleList.tail(oneElementList) ==== Nil
    }

    "for list with multiple elements" in {
      SimpleList.tail(defaultList) ==== SimpleList(2, 3, 4, 5)
    }
  }

  "Exercise 3.3 - setHead" >> {
    "for empty list" in {
      SimpleList.setHead(emptyList, 8) ==== emptyList
    }

    "for list with 1 element" in {
      SimpleList.setHead(oneElementList, 8) ==== SimpleList(8)
    }

    "for list with multiple elements" in {
      SimpleList.setHead(defaultList, 8) ==== SimpleList(8, 2, 3, 4, 5)
    }
  }

  "Exercise 3.4 - drop" in {
    SimpleList.drop(emptyList, 10) ==== emptyList
    SimpleList.drop(oneElementList, 1) ==== emptyList
    SimpleList.drop(defaultList, 1) ==== SimpleList(2, 3, 4, 5)
    SimpleList.drop(defaultList, 2) ==== SimpleList(3, 4, 5)
    SimpleList.drop(defaultList, 4) ==== SimpleList(5)
  }

  "Exercise 3.5 - dropWhile" in {
    val isEven: Int => Boolean = (x) => x % 2 == 0
    SimpleList.dropWhile(defaultList, isEven) ==== SimpleList(1, 3, 5)
    SimpleList.dropWhile(defaultList, (x: Int) => false) ==== defaultList
    SimpleList.dropWhile(defaultList, (x: Int) => true) ==== emptyList
  }

  "Exercise 3.5 - dropWhileCurried" in {
    val isEven: Int => Boolean = (x) => x % 2 == 0
    SimpleList.dropWhileCurried(defaultList)(isEven) ==== SimpleList(1, 3, 5)
    SimpleList.dropWhileCurried(defaultList)(x => false) ==== defaultList
    SimpleList.dropWhileCurried(defaultList)(_ => true) ==== emptyList
  }

  "Exercise 3.6 - init" in {
    SimpleList.init(emptyList) ==== emptyList
    SimpleList.init(oneElementList) ==== emptyList
    SimpleList.init(defaultList) ==== SimpleList(1, 2, 3, 4)
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

    SimpleList.foldRight(defaultList, Nil: SimpleList[Int])(Cons(_, _)) ==== defaultList
  }

  "Exercise 3.9 - length" in {
    SimpleList.length(emptyList) ==== 0
    SimpleList.length(oneElementList) ==== 1
    SimpleList.length(defaultList) ==== 5
  }

  "Exercise 3.10 - tail-recursive foldLeft" in {
    /*
    Why foldRight is not tail-recursive

    We cannot store the evaluation of f and the rest of the list in advance. And we cannot annotate
    @tailrec to foldRight
     */

    ok
    // List.foldLeft(defaultList, 0)(_ + _) ==== 15
    // List.foldLeft(defaultList, 1000.0)(_ / _) ==== 8
    // List.foldRight(defaultList, 1000.0)(_ / _) ==== 8
  }

  "Exercise 3.11 - lengthWithFoldLeft" in {
    SimpleList.lengthWithFoldLeft(defaultList) ==== SimpleList.length(defaultList)
  }

  "Exercise 3.12 - reverse" in {
    SimpleList.reverse(defaultList) ==== SimpleList(5, 4, 3, 2, 1)
  }

  "Exercise 3.13 - write foldLeft in terms of foldRight" in {
    // foldLeft and foldRight are the same for associative + commutative function
    SimpleList.foldLeftInTermsOfFoldRight(defaultList, 0)(_ + _) ==== SimpleList.foldLeft(defaultList, 0)(_ + _)

    // string concatenation is associative, but not commutative
    // Hence string concatenation is a good method to test
    SimpleList.foldLeftInTermsOfFoldRight(strList, "")(_ + _) ==== SimpleList.foldLeft(strList, "")(_ + _)
    ok
  }

  "Exercise 3.13 - write foldRight in terms of foldLeft" in {
    /*
    foldRight(List("a", "b"), zeroR)(fR)(z)

    Suppose that z is "prefix"

    fR(
      "a",
      fR(
        "b",
        foldRight(Nil, zeroR)(fR)
      )
    )("prefix")

    ===> then we have
    fR(
      "a",
      fR("b", zeroR)
    )("prefix")

    ===> then we have
    fR(
      "a",
      (b1: B) => zeroR(f(b1, "b"))
    )("prefix")

    ===> then we have
    (
      (b2: B) =>
        (
          (b1: B) => zeroR(f(b1, "b"))
        ) (f(b2, "a"))
    )("prefix")

    ===> then replacing b2 with "prefix", we have:
    (
      (b1: B) => zeroR(f(b1, "b"))
    ) (f("prefix", "a"))

    ===> then we have
    (
      (b1: B) => zeroR(f(b1, "b"))
    ) ("prefixa")

    ==> then replacing b1 with "prefixa", we have
    zeroR(f("prefixa", "b"))

    ==> Finally
    zeroR("prefixab")

    ==> replace zeroR with its definition
    ((b: B) => b) ("prefixab")

    ==> Replacing b with "prefixab"
    "prefixab"

     */
    ok
  }

  "Exericise 3.14 - append in terms of foldRight" in {
    SimpleList.append(defaultList, additionalList) ==== SimpleList.appendWithFoldRight(defaultList, additionalList)
  }

  "Exericise 3.14 - append in terms of foldLeft" in {
    SimpleList.append(defaultList, additionalList) ==== SimpleList.appendWithFoldLeft(defaultList, additionalList)
    SimpleList.appendWithFoldLeft(defaultList, additionalList) ==== SimpleList(1, 2, 3, 4, 5, 10, 11, 12)
  }

  "Exercise 3.15 - concatenates a list of lists into a single list with linear runtime" in {
    val listOfLists = SimpleList(defaultList, additionalList, SimpleList(21, 22))
    SimpleList.concatenate(listOfLists) ==== SimpleList(1, 2, 3, 4, 5, 10, 11, 12, 21, 22)
  }

  "Exercise 3.16 - Adding 1" in {
    SimpleList.addOne(defaultList) ==== SimpleList(2, 3, 4, 5, 6)
  }

  "Exercise 3.17 - Adding 1" in {
    SimpleList.listDoubleToListString(SimpleList(1.3, 2.16, 10.20)) ==== SimpleList("1.3", "2.16", "10.2")
  }

  "Exercise 3.18 - map" in {
    SimpleList.map(defaultList)(_ + 1) ==== SimpleList(2, 3, 4, 5, 6)
  }

  "Exercise 3.19 - filter" in {
    SimpleList.filter1(defaultList)(isEvenFn) ==== SimpleList(2, 4)
    SimpleList.filter(defaultList)(isEvenFn) ==== SimpleList(2, 4)
  }

  "Exercise 3.20 - flatMap" in {
    SimpleList.flatMap(defaultList)(i => SimpleList(i, i)) ==== SimpleList(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
  }

  "Exercise 3.21 - filterWithFlatMap" in {
    SimpleList.filterWithFlatMap(defaultList)(isEvenFn) ==== SimpleList.filter(defaultList)(isEvenFn)
  }

  "Exercise 3.22 - adding corresponding elements from 2 lists" in {
    SimpleList.addTwoList(SimpleList(1, 2, 3), SimpleList(4, 5, 6)) ==== SimpleList(5, 7, 9)
    SimpleList.addTwoList(SimpleList(1, 2, 3, 4, 5), SimpleList(4, 5, 6)) ==== SimpleList(5, 7, 9)
    SimpleList.addTwoList(SimpleList(1, 2, 3), SimpleList(4, 5, 6, 7, 8, 9)) ==== SimpleList(5, 7, 9)
  }

  "Exercise 3.23 - zipWith" in {
    SimpleList.zipWith(defaultList, additionalList)(_ + _) ==== SimpleList(11, 13, 15)
    SimpleList.zipWith(SimpleList("a", "b"), SimpleList("1", "2"))((a1, a2) => s"$a1 : $a2") ==== SimpleList(
      "a : 1",
      "b : 2"
    )
  }

  "Exercise 3.24 - remove" in {
    SimpleList.remove(defaultList, 2) ==== SimpleList(1, 3, 4, 5)
    SimpleList.remove(defaultList, 3) ==== SimpleList(1, 2, 4, 5)
    SimpleList.remove(defaultList, 5) ==== SimpleList(1, 2, 3, 4)
    SimpleList.remove(SimpleList(2, 2, 2), 2) ==== SimpleList(2, 2)
    SimpleList.remove(defaultList, 10) ==== defaultList
    SimpleList.remove(SimpleList(), 10) ==== SimpleList()
  }

  "Exercise 3.24 - hasSubsequence" in {
    SimpleList.hasSubsequence(defaultList, SimpleList()) ==== true
    SimpleList.hasSubsequence(defaultList, SimpleList(1)) ==== true
    SimpleList.hasSubsequence(defaultList, SimpleList(1, 2, 3)) ==== true
    SimpleList.hasSubsequence(defaultList, SimpleList(1, 10)) ==== false
  }

}
