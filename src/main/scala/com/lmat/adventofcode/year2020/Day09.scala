package com.lmat.adventofcode.year2020

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.util.Try

object Day09 extends SimpleCommonPuzzle[Seq[Int], Int, Int] {
  override def parse(resource: String): Seq[Int] =
    readResource(resource).flatMap(row => Try(row.toInt).toOption)

  def isSumOfPrevious(target: Int, seq: Seq[Int]): Boolean =
    seq.combinations(2).map(_.sum).contains(target)

  override def part1(input: Seq[Int]): Int =
    input
      .sliding(26)
      .find(l => !isSumOfPrevious(l.last, l))
      .map(_.last)
      .get

  override def part2(input: Seq[Int]): Int = {
    val target = part1(input)
    LazyList
      .from(2)
      .flatMap(input.sliding)
      .find(_.sum == target)
      .map(s => s.min + s.max)
      .get
  }
}
