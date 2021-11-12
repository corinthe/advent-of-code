package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.util.Try

object Day02 extends SimpleCommonPuzzle[Seq[Seq[Int]], Int, Int]{
  override def parse(resource: String): Seq[Seq[Int]] =
    readResource(resource)
      .map(str => str.split("\t").flatMap(x => Try(x.toInt).toOption).toSeq)

  override def part1(input: Seq[Seq[Int]]): Int = input
    .map(row => row.max - row.min)
    .sum

  override def part2(input: Seq[Seq[Int]]): Int =
    input
      .flatMap(row => row.combinations(2))
      .map(seq => seq.max.toFloat / seq.min)
      .filter(_.isWhole)
      .sum.toInt
}
