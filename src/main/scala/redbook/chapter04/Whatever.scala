package redbook.chapter04

sealed trait Whatever[+E, +A] {
  def map[B](f: A => B): Whatever[E, B] = this match {
    case Success(a) => Success(f(a))
    case Errors(e)  => Errors(e)
  }

  def flatMap[EE >: E, B >: A](f: A => Whatever[EE, B]): Whatever[EE, B] = this match {
    case Success(a) => f(a)
    case Errors(e)  => Errors(e)
  }

  def orElse[EE >: E, B >: A](b: => Whatever[EE, B]): Whatever[EE, B] = this match {
    case Success(a) => Success(a)
    case Errors(e)  => b
  }

  // TODO: check why the for comprehension doesn't work here
  def map2[EE >: E, B, C](b: Whatever[EE, B])(f: (A, B) => C): Whatever[EE, C] = {
    (this, b) match {
      case (Success(aa), Success(bb)) => Success(f(aa, bb))
      case (Success(_), Errors(e))    => Errors(e)
      case (Errors(e), Success(_))    => Errors(e)
      case (Errors(e1), Errors(e2))           => Errors(e1 :++ e2)
    }
  }
}

case class Errors[+E](values: List[E]) extends Whatever[E, Nothing]
case class Success[+A](value: A)       extends Whatever[Nothing, A]

object Person {
  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)

  def mkName(name: String): Whatever[String, Name] =
    if (name == "" || name == null) Errors(List("Name is empty."))
    else Success(new Name(name))

  def mkAge(age: Int): Whatever[String, Age] =
    if (age < 0) Errors(List("Age is out of range."))
    else Success(new Age(age))

  def mkPerson(name: String, age: Int): Whatever[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))
}
