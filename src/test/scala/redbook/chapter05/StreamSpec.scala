package redbook.chapter05

import org.specs2.mutable.Specification

class StreamSpec extends Specification {
  object Fixture {
    def expensiveCalculation(value: Int): Int = {
      println(s"expensive $value")
      value
    }

    val s1 = Stream.apply(expensiveCalculation(1), expensiveCalculation(2), expensiveCalculation(3))
  }

  import Fixture._

  "Exercise 5.1 - toList" in {
    s1.toList ==== List(1, 2, 3)
  }

  "Exercise 5.2 - take" in {
    s1.take(1).toList ==== List(1)
    s1.take(2).toList ==== List(1, 2)
    s1.take(6).toList ==== List(1, 2, 3)
    s1.take(-1).toList ==== Nil
  }

  "Exercise 5.2 - drop" in {
    s1.drop(6).toList ==== Nil
    s1.drop(3).toList ==== Nil
    s1.drop(2).toList ==== List(3)
    s1.drop(1).toList ==== List(2, 3)
    s1.drop(0).toList ==== List(1, 2, 3)
    s1.drop(-1).toList ==== List(1, 2, 3)
  }

  "Exercise 5.3 - takeWhile" in {
    def isOdd(n: Int) = n % 2 == 1

    s1.takeWhile(isOdd).toList ==== List(1, 3)
  }
}
