package redbook.chapter03

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil                             extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def tail[A](as: List[A]): List[A] = as match {
    case Nil         => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](as: List[A], newHeadValue: A): List[A] = as match {
    case Nil         => Nil
    case Cons(x, xs) => Cons(newHeadValue, xs)
  }

  def drop[A](as: List[A], n: Int): List[A] = as match {
    case Nil                  => Nil
    case Cons(x, xs) if n > 0 => drop(xs, n - 1)
    case _                    => as
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil         => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else Cons(x, dropWhile(xs, f))
  }

  def dropWhileCurried[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil         => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else Cons(x, dropWhile(xs, f))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil        => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil          => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = {
    @tailrec
    def helper[A](as: List[A], n: Int): Int = as match {
      case Nil        => n
      case Cons(h, t) => helper(t, n + 1)
    }

    helper(as, 0)
  }

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil        => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sumWithFoldLeft(ns: List[Int]): Int        = foldLeft(ns, 0)(_ + _)
  def productWithFoldLeft(ns: List[Int]): Double = foldLeft(ns, 1.0)(_ * _)
  def lengthWithFoldLeft[A](l: List[A]): Int     = foldLeft(l, 0)((b, _) => b + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))

  // MARK: I still don't really understand
  def foldLeftInTermsOfFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    type ZeroR = B => B

    val zeroR: ZeroR               = (b: B) => b
    def fR(a: A, zR: ZeroR): ZeroR = (b: B) => zR(f(b, a))

    foldRight(as, zeroR)(fR)(z)
  }

  def foldLeftWithReverseAndFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B) =
    foldRight(List.reverse(as), z)((a, b) => f(b, a))

  def appendWithFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((a, b) => Cons(a, b))

  def appendWithFoldLeft[A](a1: List[A], a2: List[A]): List[A] = foldLeft(List.reverse(a1), a2)((b, a) => Cons(a, b))

  def concatenate[A](listOfLists: List[List[A]]): List[A] = {
    foldLeft(List.reverse(listOfLists), Nil: List[A])((acc, ls) => appendWithFoldLeft(ls, acc))
  }
}
