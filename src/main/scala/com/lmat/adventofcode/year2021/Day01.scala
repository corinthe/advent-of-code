package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.util.Try

object Day01 extends SimpleCommonPuzzle[Seq[Int], Int, Int] {

  override def parse(resource: String): Seq[Int] =
    readResource(resource).flatMap(row => Try(row.toInt).toOption)

  override def part1(depths: Seq[Int]): Int =
    depths.sliding(2).count(two => two.head < two.last)


  override def part2(depths: Seq[Int]): Int =
    depths
      .sliding(3)
      .map(_.sum)
      .sliding(2)
      .count(t => t.head < t.last)

}
