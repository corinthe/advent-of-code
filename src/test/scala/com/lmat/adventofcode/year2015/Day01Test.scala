package com.lmat.adventofcode.year2015

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day01Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val plans =
    Table(
      ("plan", "floor"),
      ("(())",     0),
      ("()()",     0),
      ("(((",      3),
      ("(()(()(",  3),
      ("))(((((",  3),
      ("())",     -1),
      ("))(",     -1),
      (")))",     -3),
      (")())())", -3)
    )

  test("Day01 - Part 1") {
    forAll(plans) { (plan, floor) =>
      assert(part1(plan) == floor)
    }
  }

  val plans2 =
    Table(
      ("plan",   "position"),
      (")",      1),
      ("()())",  5)
    )

  test("Day01 - Part 2") {
    forAll(plans2) { (plan, position) =>
      assert(part2(plan) == position)
    }
  }
}
