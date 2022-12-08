package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource
import com.lmat.util.StringExtensions.RichString

object Day04 extends SimpleCommonPuzzle[String, Int, Int] {
  def solve(s: String, trailingZeroes: Int): Int =
    LazyList.from(0).zipWithIndex
      .map { case (value, i) => ((s + value).md5, i) }
      .filter { case (value, _) => value.take(trailingZeroes).forall(_ == '0') }
      .head
      ._2


  override def parse(resource: String): String =
    readResource(resource).head

  override def part1(input: String): Int = {
    solve(input, 5)
  }

  override def part2(input: String): Int =
    solve(input, 6)
}
