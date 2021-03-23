package redbook.chapter03

import scala.annotation.tailrec

sealed trait SimpleList[+A]

case object Nil                                   extends SimpleList[Nothing]
case class Cons[+A](head: A, tail: SimpleList[A]) extends SimpleList[A]

object SimpleList {

  def apply[A](as: A*): SimpleList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: SimpleList[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: SimpleList[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def tail[A](as: SimpleList[A]): SimpleList[A] = as match {
    case Nil         => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](as: SimpleList[A], newHeadValue: A): SimpleList[A] = as match {
    case Nil         => Nil
    case Cons(x, xs) => Cons(newHeadValue, xs)
  }

  def drop[A](as: SimpleList[A], n: Int): SimpleList[A] = as match {
    case Nil                  => Nil
    case Cons(x, xs) if n > 0 => drop(xs, n - 1)
    case _                    => as
  }

  def dropWhile[A](l: SimpleList[A], f: A => Boolean): SimpleList[A] = l match {
    case Nil         => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else Cons(x, dropWhile(xs, f))
  }

  def dropWhileCurried[A](l: SimpleList[A])(f: A => Boolean): SimpleList[A] = l match {
    case Nil         => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else Cons(x, dropWhile(xs, f))
  }

  // MARK: not tail recursive
  def append[A](a1: SimpleList[A], a2: SimpleList[A]): SimpleList[A] = a1 match {
    case Nil        => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](l: SimpleList[A]): SimpleList[A] = l match {
    case Nil          => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))
  }

  def foldRight[A, B](as: SimpleList[A], z: B)(f: (A, B) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: SimpleList[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: SimpleList[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: SimpleList[A]): Int = {
    @tailrec
    def helper[A](as: SimpleList[A], n: Int): Int = as match {
      case Nil        => n
      case Cons(h, t) => helper(t, n + 1)
    }

    helper(as, 0)
  }

  @tailrec
  def foldLeft[A, B](as: SimpleList[A], z: B)(f: (B, A) => B): B = as match {
    case Nil        => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sumWithFoldLeft(ns: SimpleList[Int]): Int        = foldLeft(ns, 0)(_ + _)
  def productWithFoldLeft(ns: SimpleList[Int]): Double = foldLeft(ns, 1.0)(_ * _)
  def lengthWithFoldLeft[A](l: SimpleList[A]): Int     = foldLeft(l, 0)((b, _) => b + 1)

  def reverse[A](l: SimpleList[A]): SimpleList[A] = foldLeft(l, Nil: SimpleList[A])((b, a) => Cons(a, b))

  // MARK: I still don't really understand
  def foldLeftInTermsOfFoldRight[A, B](as: SimpleList[A], z: B)(f: (B, A) => B): B = {
    type ZeroR = B => B

    val zeroR: ZeroR               = (b: B) => b
    def fR(a: A, zR: ZeroR): ZeroR = (b: B) => zR(f(b, a))

    foldRight(as, zeroR)(fR)(z)
  }

  def foldLeftWithReverseAndFoldRight[A, B](as: SimpleList[A], z: B)(f: (B, A) => B) =
    foldRight(SimpleList.reverse(as), z)((a, b) => f(b, a))

  def appendWithFoldRight[A](a1: SimpleList[A], a2: SimpleList[A]): SimpleList[A] =
    foldRight(a1, a2)((a, b) => Cons(a, b))

  def appendWithFoldLeft[A](a1: SimpleList[A], a2: SimpleList[A]): SimpleList[A] =
    foldLeft(SimpleList.reverse(a1), a2)((b, a) => Cons(a, b))

  // 3.15
  def concatenate[A](listOfLists: SimpleList[SimpleList[A]]): SimpleList[A] = {
    foldLeft(SimpleList.reverse(listOfLists), Nil: SimpleList[A])((acc, ls) => appendWithFoldLeft(ls, acc))
  }

  // 3.16
  def addOne[A](as: SimpleList[A])(implicit num: Numeric[A]): SimpleList[A] =
    foldRight(as, Nil: SimpleList[A])((a, acc) => Cons(num.plus(a, num.one), acc))

  // 3.17
  def listDoubleToListString(as: SimpleList[Double]): SimpleList[String] =
    foldRight(as, Nil: SimpleList[String])((a, acc) => Cons(a.toString, acc))

  // 3.18
  def map[A, B](as: SimpleList[A])(f: A => B): SimpleList[B] =
    foldRight(as, Nil: SimpleList[B])((head, tail) => Cons(f(head), tail))

  // 3.19
  def filter1[A](as: SimpleList[A])(f: A => Boolean): SimpleList[A] = {
    def helper(as: SimpleList[A], result: SimpleList[A]): SimpleList[A] = as match {
      case Nil        => result
      case Cons(h, t) => if (f(h)) helper(t, Cons(h, result)) else helper(t, result)
    }

    reverse(helper(as, Nil))
  }

  def filter[A](as: SimpleList[A])(f: A => Boolean) =
    foldRight(as, Nil: SimpleList[A])((a, acc) => if (f(a)) Cons(a, acc) else acc)

  // 3.20
  def flatMap[A, B](as: SimpleList[A])(f: A => SimpleList[B]): SimpleList[B] =
    foldRight(as, Nil: SimpleList[B])((a, acc) => append(f(a), acc))

  // 3.21
  def filterWithFlatMap[A](as: SimpleList[A])(f: A => Boolean) = flatMap(as)(a => if (f(a)) SimpleList(a) else Nil)

  // 3.22
  def addTwoList[A](l1: SimpleList[A], l2: SimpleList[A])(implicit num: Numeric[A]): SimpleList[A] = {
    def helper(l1: SimpleList[A], l2: SimpleList[A], result: SimpleList[A]): SimpleList[A] = (l1, l2) match {
      case (Nil, _)                     => result
      case (_, Nil)                     => result
      case (Cons(h1, t1), Cons(h2, t2)) => helper(t1, t2, Cons(num.plus(h1, h2), result))
    }
    reverse(helper(l1, l2, Nil: SimpleList[A]))
  }

  // 3.23
  def zipWith[A](l1: SimpleList[A], l2: SimpleList[A])(f: (A, A) => A): SimpleList[A] = {
    def helper(l1: SimpleList[A], l2: SimpleList[A], result: SimpleList[(A, A)]): SimpleList[(A, A)] = (l1, l2) match {
      case (Nil, _)                     => result
      case (_, Nil)                     => result
      case (Cons(h1, t1), Cons(h2, t2)) => helper(t1, t2, Cons((h1, h2), result))
    }

    map(reverse(helper(l1, l2, Nil: SimpleList[(A, A)])))(x => x match { case (a1, a2) => f(a1, a2) })
  }

  // 3.24
  def exists[A](as: SimpleList[A], item: A): Boolean = as match {
    case Nil        => false
    case Cons(h, t) => if (h == item) true else exists(t, item)
  }

  def remove[A](as: SimpleList[A], item: A): SimpleList[A] = {
    def helper[A](as: SimpleList[A], item: A, result: SimpleList[A]): SimpleList[A] = as match {
      case Nil => result
      case Cons(h, t) =>
        if (h == item)
          SimpleList.append(SimpleList.reverse(t), result)
        else
          helper(t, item, Cons(h, result))
    }

    SimpleList.reverse(helper(as, item, Nil: SimpleList[A]))
  }

  def hasSubsequence[A](sup: SimpleList[A], sub: SimpleList[A]): Boolean = {
    def helper[A](sup: SimpleList[A], sub: SimpleList[A]): Boolean = sub match {
      case Nil => true
      case Cons(h, t) => {
        val removedSup = remove(sup, h)
        if (removedSup == sup) false
        else helper(removedSup, t)
      }
    }

    helper(sup, sub)
  }

}
