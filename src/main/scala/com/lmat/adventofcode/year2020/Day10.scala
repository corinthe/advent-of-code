package com.lmat.adventofcode.year2020

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource


object Day10 extends SimpleCommonPuzzle[Seq[Int], Int, Long] {
  override def parse(resource: String): Seq[Int] = {
    val input = readResource(resource).map(_.toInt).sorted
    input.appended(input.last + 3).prepended(0)
  }

  override def part1(input: Seq[Int]): Int = {
    val by1 = input.sliding(2).count(s => s.last - s.head == 1)
    val by3 = input.sliding(2).count(s => s.last - s.head == 3)

    by1 * by3
  }

  def tribonacci(seqLength: Int): Long = seqLength match {
    case l if l > 1 => Math.round(1.84 * tribonacci(l - 1))
    case 1 => 1
  }

  def toStep(input: Seq[Int]): Seq[Int] =
    input.sliding(2).map(s => s.last - s.head).toSeq

  def groupOneSize(input: Seq[Int]): Seq[Int] = {
    input.mkString("").split("3").map(_.length)
  }

  override def part2(input: Seq[Int]): Long =
    groupOneSize(
      toStep(input)
    ).filter(_ > 1)
      .map(tribonacci)
      .product
}
