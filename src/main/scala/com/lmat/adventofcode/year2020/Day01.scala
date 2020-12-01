package com.lmat.adventofcode.year2020

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.util.Try

object Day01 extends SimpleCommonPuzzle[Seq[Int], Int, Int] {
  val TARGET_SUM: Int = 2020

  override def parse(resource: String): Seq[Int] =
    readResource(resource).flatMap(row => Try(row.toInt).toOption)

  override def part1(expanses: Seq[Int]): Int = {
    expanses
      .combinations(2)
      .find(duo => duo.sum == TARGET_SUM)
      .map(duo => duo.product)
      .getOrElse(0)
  }

  override def part2(expanses: Seq[Int]): Int =
    expanses
      .combinations(3)
      .find(combination => combination.sum == TARGET_SUM)
      .map(combination => combination.product)
      .getOrElse(0)
}
