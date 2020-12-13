package com.lmat.adventofcode

object PuzzleRunner extends App {
  val year = args.headOption.map(_.toInt).getOrElse(2020)
  val day  = args.lift(1).map(_.toInt).getOrElse(9)

  def puzzleMap = Map(

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
