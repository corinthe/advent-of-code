package com.lmat.adventofcode.year2020

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2020.Day05Definitions.Seat
import com.lmat.util.Files.readResource

object Day05Definitions {

  case class Seat(row: Int, column: Int)

}

object Day05 extends SimpleCommonPuzzle[Seq[Int], Int, Int] {

  override def parse(resource: String): Seq[Int] = readResource(resource).map(parseSeat).map(toSeatId).sorted

  def parseSeat(input: String): Seat =
    input.splitAt(7) match {
      case (row, col) => Seat(
        Integer.parseInt(row.replaceAll("F", "0").replaceAll("B", "1"), 2),
        Integer.parseInt(col.replaceAll("L", "0").replaceAll("R", "1"), 2),
      )
    }

  def toSeatId(seat: Seat): Int = (seat.row * 8) + seat.column

  override def part1(input: Seq[Int]): Int = input.last

  override def part2(input: Seq[Int]): Int = (input.head to input.last).diff(input).head
}
