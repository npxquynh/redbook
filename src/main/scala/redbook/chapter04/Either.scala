package redbook.chapter04

// Exercise 4.6
sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e)  => Left(e)
  }

  def flatMap[EE >: E, B >: A](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e)  => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case Left(e)  => b
  }

  // TODO: check why the for comprehension doesn't work here
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    // for {
    //   a  <- this
    //   bb <- b
    // } yield f(a, bb)

    (this, b) match {
      case (Right(aa), Right(bb)) => Right(f(aa, bb))
      case (Right(_), Left(e))    => Left(e)
      case (Left(e), _)           => Left(e)
    }
  }
}

case class Left[+E](value: E)  extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  // Exercise 4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    val z: Either[E, List[A]] = Right(List.empty[A])
    val f: (Either[E, A], Either[E, List[A]]) => Either[E, List[A]] = (ea, acc) => {
      ea.map2(acc)((a, as) => a :: as)
    }

    es.foldRight(z)(f)
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = sequence(as.map(f))
}
