package redbook.chapter04

import org.specs2.mutable.Specification

class WhateverSpec extends Specification {
  "Exercise 4.8 - map2 that collects all the errors" in {
    Person.mkPerson("", 10) ==== Errors(List(
      "Name is empty."
    ))

    Person.mkPerson("xxx", -1) ==== Errors(List(
      "Age is out of range."
    ))

    Person.mkPerson("", -1) ==== Errors(List(
      "Name is empty.",
      "Age is out of range."
    ))
  }

}
