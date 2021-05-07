package redbook.chapter04

trait Option[+A] {

  // Exercise 4.1
  def map[B](f: A => B): Option[B] = this match {
    case None    => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case None    => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    val fn: A => Option[B] = a => Some(a)
    this.map(fn).getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = this.flatMap(a => if (f(a)) Some(a) else None)

}

case class Some[+A](get: A) extends Option[A]
case object None            extends Option[Nothing]

object Utils {

  // Exercise 4.2
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // This is the population variance
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  // Exercise 4.3
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  // val lift2[A,B,C](f: (A, B) => C) => (Option[A], Option[B]) => Option[C] =
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(aValue), Some(bValue)) => Some(f(aValue, bValue))
    case _                            => None
  }

  // Exercise 4.4
  def sequenceOld[A](as: List[Option[A]]): Option[List[A]] = {
    val z: Option[List[A]] = Some(List.empty[A])
    val fn: (Option[A], Option[List[A]]) => Option[List[A]] = (optionA, optionList) =>
      map2(optionA, optionList)((a, l) => a :: l)

    as.foldRight(z)(fn)
  }

  def sequence[A](as: List[Option[A]]): Option[List[A]] = {
    val z: Option[List[A]] = Some(List.empty[A])
    val fn: (Option[A], Option[List[A]]) => Option[List[A]] = (optionA, optionList) =>
      map2(optionA, optionList)((a, l) => a :: l)

    as.foldRight(z)(fn)
  }

  // Exercise 4.5
  def simpleTraverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = sequence(as.map(f))

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    val z: Option[List[B]] = Some(List.empty[B])

    as.foldRight(z)((a, acc) => map2(f(a), acc)(_ :: _))
  }

  def sequenceInTermOfTraverse[A](as: List[Option[A]]): Option[List[A]] = traverse(as)(identity)
}
