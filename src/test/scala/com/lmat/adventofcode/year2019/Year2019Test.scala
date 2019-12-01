package com.lmat.adventofcode.year2019
import com.lmat.adventofcode.PuzzleRunner.{puzzleMap, resource}
import org.scalatest.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Year2019Test extends FunSuite with TableDrivenPropertyChecks {

  val puzzles =
    Table(
      ("day", "part1", "part2"),
      (1,     3315133, 4969831)
    )

  forAll(puzzles) { (day, part1, part2) =>
    val year = 2019

    test(s"$year: Day $day") {
      val (res1, res2) = puzzleMap(year, day).solve(resource(year, day))
      assert(res1 == part1)
      assert(res2 == part2)
    }
  }
}


