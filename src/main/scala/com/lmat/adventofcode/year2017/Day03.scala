package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.util.Try

object Day03 extends SimpleCommonPuzzle[Int, Int, Int]{
  override def parse(resource: String): Int =
    readResource(resource).head.toInt

  override def part1(input: Int): Int = {

    0
  }

  override def part2(input: Int): Int = 0
}
