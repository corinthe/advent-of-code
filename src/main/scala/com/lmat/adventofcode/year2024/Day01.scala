package com.lmat.adventofcode.year2024

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

object Day01 extends SimpleCommonPuzzle[(Seq[Int], Seq[Int]), Int, Int] {

  override def parse(resource: String): (Seq[Int], Seq[Int]) =
    readResource(resource)
      .map(l => l.split("   ").map(_.toInt))
      .map(arr => (arr.head, arr.last))
      .unzip

  override def part1(input: (Seq[Int], Seq[Int])): Int = {
    input._1.sorted.zip(input._2.sorted).map {case (a,b) => Math.abs(a - b) }.sum
  }

  override def part2(input: (Seq[Int], Seq[Int])): Int = {
    val occurences = input._2.groupMapReduce(identity)(_ => 1)(_ + _)
    input._1.map(elem => elem * occurences.getOrElse(elem, 0)).sum
  }
}
