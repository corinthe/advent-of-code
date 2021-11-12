package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day01.{part1, part2}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day01Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val captchas1 =
    Table(
      ("captchas",   "result"),
      (Seq(1, 1, 2, 2),  3),
      (Seq(1, 1, 1, 1),  4),
      (Seq(9, 1, 2, 1, 2, 1, 2, 9), 9),
    )

  test("Day01 - Part 1") {
    forAll(captchas1) { (captcha, result) =>
      assert(part1(captcha) == result)
    }
  }

  val captchas2 =
    Table(
      ("captchas",   "result"),
      (Seq(1, 2, 1, 2),  6),
      (Seq(1, 2, 2, 1),  0),
      (Seq(1, 2, 3, 4, 2, 5), 4),
      (Seq(1, 2, 3, 1, 2, 3), 12),
      (Seq(1, 2, 1, 3, 1, 4, 1, 5), 4),
    )

  test("Day01 - Part 2") {
    forAll(captchas2) { (captcha, result) =>
      assert(part2(captcha) == result)
    }
  }

}
