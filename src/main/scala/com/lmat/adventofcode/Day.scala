package com.lmat.adventofcode

import com.lmat.util.Files.readResource

import scala.util.Try

object Day extends SimpleCommonPuzzle[Seq[String], Int, Int] {
  override def parse(resource: String): Seq[String] =
    readResource(resource)

  override def part1(input: Seq[String]): Int = ???

  override def part2(input: Seq[String]): Int = ???
}
