package com.lmat.adventofcode

object PuzzleRunner extends App {
  val year = args.headOption.map(_.toInt).getOrElse(2019)
  val day  = args.lift(1).map(_.toInt).getOrElse(1)

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
