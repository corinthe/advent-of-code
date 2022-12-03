package com.lmat.adventofcode.year2022

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource


object Day01 extends SimpleCommonPuzzle[Seq[Seq[Int]], Int, Int] {
  override def parse(resource: String): Seq[Seq[Int]] = {
    readResource(resource).foldLeft(Seq[Seq[Int]]() :+ Seq())({ case (acc, s) => s match {
      case x if x == null || x.isEmpty => acc :+ Seq[Int]()
      case z =>
        val updated = acc.last.appended(z.toInt)
        acc.init :+ updated
    }})
  }

  override def part1(input: Seq[Seq[Int]]): Int =
    input.map(_.sum).max

  override def part2(input: Seq[Seq[Int]]): Int =
    input.map(_.sum).sorted.takeRight(3).sum
}
