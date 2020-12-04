package com.lmat.adventofcode.year2020

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.collection.immutable.LazyList.from


object Day03 extends SimpleCommonPuzzle[Seq[String], Int, BigInt] {
  override def parse(resource: String): Seq[String] =
    readResource(resource)

  def isTree(char: Char): Boolean = char == '#'

  def countTrees(input: Seq[String], right: Int): Int =
    input
      .zip(from(0, right))
      .count({case (line, pos) => isTree(line.charAt(pos % line.length))})

  override def part1(input: Seq[String]): Int =
    countTrees(input, 3)

  override def part2(input: Seq[String]): BigInt = {

    def filterLines(input: Seq[String], down: Int): Seq[String] =
      input
        .zipWithIndex
        .filter(_._2 % down == 0)
        .map(_._1)

    val params = List(
      (1, 1),
      (3, 1),
      (5, 1),
      (7, 1),
      (1, 2)
    )

    params
      .map({case (right, down) => (filterLines(input, down), right)})
      .map({case (lines, right) => countTrees(lines, right)})
      .map(BigInt(_))
      .product

  }

}
