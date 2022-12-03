package com.lmat.adventofcode.year2022

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

object Day03 extends SimpleCommonPuzzle[Seq[String], Int, Int] {

  private implicit class CharOps(c: Char) {
    def priority: Int = c.toLower - 'a' + (if(c.isLower) 1 else 27)
  }

  override def parse(resource: String): Seq[String] =
    readResource(resource)

  override def part1(input: Seq[String]): Int =
    input
    .map(s => s.splitAt(s.length / 2))
    .map(halves => halves._1.intersect(halves._2).head)
    .map(_.priority)
    .sum


  override def part2(input: Seq[String]): Int =
    input
      .grouped(3)
      .map(_.reduceLeft((left, right) => left.intersect(right)).head)
      .map(_.priority)
      .sum

}
