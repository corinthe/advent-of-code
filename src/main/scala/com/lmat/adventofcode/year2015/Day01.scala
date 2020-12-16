package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

object Day01 extends SimpleCommonPuzzle[String, Int, Int] {
  type Floor = Int
  override def parse(resource: String): String =
    readResource(resource).head

  override def part1(input: String): Floor = input.foldLeft(0)(direction)

  override def part2(input: String): Int =
    input
      .scanLeft(0)(direction)
      .zipWithIndex
      .filter(_._1 == -1)
      .head
      ._2

  def direction(floor: Floor, c: Char): Floor = c match {
    case '(' => floor + 1
    case ')' => floor - 1
    case _ => throw new IllegalStateException()
  }
}
