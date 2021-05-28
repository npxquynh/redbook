package redbook.chapter04

import org.specs2.mutable.Specification
import scala.util.Try
import scala.util.Failure
import scala.util.Success

class EitherSpec extends Specification {

  object Fixture {
    val error: Either[String, Int] = Left("Error")
    val value42: Either[String, Int] = Right(42)
    val value101: Either[String, Int] = Right(101)
  }

  import Fixture._

  "Exercise 4.6 - map" in {
    error.map(_ + 100) ==== error
    value42.map(_ + 100) ==== Right(142)
  }

  "Exercise 4.6 - flatMap" in {
    val f: Int => Either[String, Int] = a => {
      if (a % 2 == 0) Right(a * 100)
      else Left("Not divided by 2")
    }

    value42.flatMap(f) ==== Right(4200)
    value101.flatMap(f) ==== Left("Not divided by 2")
    error.flatMap(f) ==== Left("Error")
  }

  "Exercise 4.6 - orElse" in {
    val alwaysValid: Either[String, Long] = Right(42L)

    error.orElse(alwaysValid) ==== alwaysValid
    value42.orElse(alwaysValid) ==== value42
  }

  "Exercise 4.6 - map2" in {
    value42.map2(value101)(_ + _) ==== Right(143)
    value42.map2(error)(_ + _) ==== error
    error.map2(value42)(_ + _) ==== error
  }

  "Exercise 4.7 - sequence" in {
    val l = List(value42, value101)
    Either.sequence(l) ==== Right(List(42, 101))
  }

  "Exercise 4.7 - traverse" in {
    val invalidList = List("42", "a40", "100")
    val validList = List("1", "42", "16")
    val f: String => Either[String, Int] = s =>
      Try(Right(s.toInt)) match {
        case Failure(_) => Left(s"Cannot convert $s to Int")
        case Success(v) => v
      }

    Either.traverse(invalidList)(f) ==== Left("Cannot convert a40 to Int")
    Either.traverse(validList)(f) ==== Right(List(1, 42, 16))
  }
}
