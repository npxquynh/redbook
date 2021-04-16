package redbook.chapter04

import org.specs2.mutable.Specification

class OptionSpec extends Specification {

  object Fixture {
    val some2: Option[Int]   = Some(2)
    val some10: Option[Int]  = Some(10)
    val noneInt: Option[Int] = None
  }

  import Fixture._

  "Exercise 4.1 - flatMap" in {
    some2.flatMap(a => Some(a * 2)) ==== Some(4)
    some10.flatMap(a => Some(a * 10)) ==== Some(100)
    some10.flatMap(_ => None) ==== None
    noneInt.flatMap(a => Some(a * 10)) ==== None
  }

  "Exercise 4.1 - filter" in {
    some2.filter(_ > 5) ==== None
    some10.filter(_ > 5) ==== some10
    noneInt.filter(_ > 5) ==== None
  }

  "Exercise 4.2 - variance" in {
    val xs: Seq[Double] = Seq(1, 2, 3, 4, 5)
    Utils.variance(xs) ==== Some(2.0)
    Utils.variance(Seq.empty[Double]) ==== None
  }

  "Exercise 4.3 - map2" in {
    val addFn: (Int, Int) => Int = _ + _
    Utils.map2(Some(1), Some(3))(addFn) ==== Some(4)
    Utils.map2(None, Some(3))(addFn) ==== None
    Utils.map2(Some(1), None)(addFn) ==== None
    Utils.map2(None, None)(addFn) ==== None
  }

  "Exercise 4.4 - sequence" in {
    Utils.sequence(List(None)) ==== None
    Utils.sequence(List(Some(1), Some(2), Some(3))) ==== Some(List(1,2,3))
    Utils.sequence(List(Some(1), None, Some(3))) ==== None
    Utils.sequence(List(Some(1), None, None)) ==== None
  }
}
