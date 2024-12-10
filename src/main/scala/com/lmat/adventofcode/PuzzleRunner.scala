package com.lmat.adventofcode

object PuzzleRunner extends App {
  val year = args.headOption.map(_.toInt).getOrElse(2020)
  val day  = args.lift(1).map(_.toInt).getOrElse(9)

  def puzzleMap = Map(

    (2015, 1) -> year2015.Day01,
    (2015, 2) -> year2015.Day02,
    (2015, 3) -> year2015.Day03,
    (2015, 4) -> year2015.Day04,
    (2015, 5) -> year2015.Day05,

    (2016, 1) -> year2016.Day01,
    (2016, 2) -> year2016.Day02,

    //2017
    (2017, 1) ->year2017.Day01,
    (2017, 2) ->year2017.Day02,
    (2017, 3) ->year2017.Day03,

    //2018
    (2018, 1) -> year2018.Day01,

    // 2019
    (2019, 1)  -> year2019.Day01,
    (2019, 2)  -> year2019.Day02,
    (2019, 3)  -> year2019.Day03,

    (2020, 1) -> year2020.Day01,
    (2020, 2) -> year2020.Day02,
    (2020, 3) -> year2020.Day03,
    (2020, 4) -> year2020.Day04,
    (2020, 5) -> year2020.Day05,
    (2020, 6) -> year2020.Day06,
    (2020, 7) -> year2020.Day07,
    (2020, 8) -> year2020.Day08,
    (2020, 9) -> year2020.Day09,
    (2020, 10) -> year2020.Day10,
    (2020, 11) -> year2020.Day11,
    (2020, 12) -> year2020.Day12,
    (2020, 13) -> year2020.Day13,
    (2020, 14) -> year2020.Day14,
    (2020, 15) -> year2020.Day15,
    (2020, 16) -> year2020.Day16,
    (2020, 17) -> year2020.Day17,
    (2020, 18) -> year2020.Day18,

    (2021, 1) -> year2021.Day01,
    (2021, 2) -> year2021.Day02,
    (2021, 3) -> year2021.Day03,
    (2021, 7) -> year2021.Day07,
    (2021, 9) -> year2021.Day09,
    (2021, 14) -> year2021.Day14,
    (2021, 21) -> year2021.Day21,
    (2021, 22) -> year2021.Day22,

    (2022, 1) -> year2022.Day01,
    (2022, 2) -> year2022.Day02,
    (2022, 3) -> year2022.Day03,
    (2022, 4) -> year2022.Day04,
    (2022, 5) -> year2022.Day05,
    (2022, 6) -> year2022.Day06,
    (2022, 7) -> year2022.Day07,
    (2022, 8) -> year2022.Day08,
    (2022, 9) -> year2022.Day09,
    (2022, 10) -> year2022.Day10,
    (2022, 11) -> year2022.Day11,
    (2022, 12) -> year2022.Day12,
    (2022, 13) -> year2022.Day13,
    (2022, 15) -> year2022.Day15,
    (2022, 18) -> year2022.Day18,

    (2023, 1) -> year2023.Day01,
    (2023, 2) -> year2023.Day02,
    (2023, 4) -> year2023.Day04,
    (2023, 11) -> year2023.Day11,

    (2024, 1) -> year2024.Day01,
    (2024, 2) -> year2024.Day02,
    (2024, 3) -> year2024.Day03,
    (2024, 4) -> year2024.Day04,
    (2024, 7) -> year2024.Day07,
    (2024, 8) -> year2024.Day08,
    (2024, 9) -> year2024.Day09,
    (2024, 10) -> year2024.Day10,

  )

  run(puzzleMap, year, day)

  def resource(year: Int, day: Int): String = s"$year/Day${"%02d".format(day)}.txt"

  def run(puzzleMap: Map[(Int, Int), Puzzle[_, _, _, _, _, _]], year: Int, day: Int): Unit =
    puzzleMap.get(year, day) match {
      case None => println(s"Puzzle for Day $day (Year $year) is not yet solved!")
      case Some(puzzle) =>
        println(s"Solving puzzle for Day $day (Year $year)")

        val res = resource(year, day)
        println(s"Loading input from $res")

        val (result1, result2) = puzzle.solve(res)
        println(s"Part 1: $result1")
        println(s"Part 2: $result2")
    }
}
