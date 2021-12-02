package com.lmat.adventofcode.year2016

import com.lmat.adventofcode.year2016.Day01.{part1, part2}
import com.lmat.adventofcode.year2016.Day01Definitions.Mouvement
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day01Test extends AnyFunSuite with TableDrivenPropertyChecks {
  val mouvements =
    Table(
      ("mouvements",   "result"),
      (Seq(Mouvement("R", 2), Mouvement("R", 3)),  5),
      (Seq(Mouvement("R", 2), Mouvement("R", 2), Mouvement("R", 2)),  2),
      (Seq(Mouvement("R", 5), Mouvement("L", 5), Mouvement("R", 5), Mouvement("R", 3)), 12),
    )

  test("Day01 - Part 1") {
    forAll(mouvements) { (mouvement, result) =>
      assert(part1(mouvement) == result)
    }
  }

  /*val captchas2 =
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
  }*/

}
