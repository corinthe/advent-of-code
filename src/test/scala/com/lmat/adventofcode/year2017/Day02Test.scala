package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day02.{part1, part2}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day02Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val spreadsheets1 =
    Table(
      ("spreadsheets", "checksums"),
      (Seq(
        Seq(5, 1, 9, 5),
        Seq(7, 5, 3),
        Seq(2, 4, 6, 8)
      ), 18
      )
    )

  test("Day02 - Part 1") {
    forAll(spreadsheets1) { (spreadsheet, checksum) =>
      assert(part1(spreadsheet) == checksum)
    }
  }

  val spreadsheets2 =
    Table(
      ("spreadsheets", "checksums"),
      (Seq(
        Seq(5, 9, 2, 8),
        Seq(9, 4, 7, 3),
        Seq(3, 8, 6, 5)
      ), 9
      )
    )

  test("Day02 - Part 2") {
    forAll(spreadsheets2) { (spreadsheet, checksum) =>
      assert(part2(spreadsheet) == checksum)
    }
  }

}
