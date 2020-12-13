package com.lmat.adventofcode.year2020

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.util.Try

object Day extends SimpleCommonPuzzle[Seq[Int], Int, Int]{
  override def parse(resource: String): Seq[Int] =
    readResource(resource).flatMap(row => Try(row.toInt).toOption)

  override def part1(input: Seq[Int]): Int = ???

  override def part2(input: Seq[Int]): Int = ???
}
