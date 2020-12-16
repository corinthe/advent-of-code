package com.lmat.adventofcode.year2015

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day20Test extends AnyFunSuite with TableDrivenPropertyChecks {
  test("Day20 - Part 1") {
    assert(part1(100)  == 6)
    assert(part1(1000) == 48)
  }

  test("Day20 - Part 2") {
    assert(part2(100)  == 6)
    assert(part2(1000) == 36)
  }
}
