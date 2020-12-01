package com.lmat.adventofcode.year2020

import com.lmat.adventofcode.year2020.Day01.{part1, part2}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day01Test extends AnyFunSuite with TableDrivenPropertyChecks {

  val expenses = Table(
    ("expense", "result1", "result2"),
    (Seq(1721, 979, 366, 299, 675, 1456), 514579, 241861950),
    (Seq(0, 2020, 1, 2019), 0, 0)
  )

  test("Day01 - Part 1") {
    forAll(expenses) {
      (expense, result1, _) => assert(part1(expense) == result1)
    }
  }

  test("Day01 - Part 2") {
    forAll(expenses) {
      (expense, _, result2) => assert(part2(expense) == result2)
    }
  }

}
