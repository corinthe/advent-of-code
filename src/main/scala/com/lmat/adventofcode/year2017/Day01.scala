package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.util.Try

object Day01 extends SimpleCommonPuzzle[Seq[Int], Int, Int]{
  override def parse(resource: String): Seq[Int] =
    readResource(resource)
      .flatMap(row => row.split("").toSeq)
      .flatMap(str => Try(str.toInt).toOption)

  override def part1(input: Seq[Int]): Int =
    (input :+ input.head)
      .sliding(2)
      .filter(group => group.head == group.last)
      .map(group => group.head)
      .sum

  override def part2(input: Seq[Int]): Int =
    (input ++ input)
      .sliding((input.length / 2) + 1)
      .zipWithIndex
      .takeWhile(p => p._2 < input.length)
      .filter(p => p._1.head == p._1.last)
      .map(p => p._1.head)
      .sum

}
