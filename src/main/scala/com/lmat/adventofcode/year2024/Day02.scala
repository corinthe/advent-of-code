package com.lmat.adventofcode.year2024

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2024.Day02Definitions.Report
import com.lmat.util.Files.readResource


object Day02Definitions {

  case class Report(levels: Seq[Int]) {
    def isSafe: Boolean = (increasing(levels) || decreasing(levels)) && step(levels)

    def dampened: LazyList[Report] = levels.indices.to(LazyList).map { index =>
      Report(levels.patch(index, Nil, 1))
    }
  }

  private def increasing(s: Seq[Int]): Boolean = s.sliding(2).forall(seq => seq.head < seq.tail.head)

  private def decreasing(s: Seq[Int]): Boolean = s.sliding(2).forall(seq => seq.head > seq.tail.head)

  private def step(s: Seq[Int]): Boolean = s.sliding(2).forall(seq => Math.abs(seq.head - seq.tail.head) >= 1 && Math.abs(seq.head - seq.tail.head) <= 3)
}

object Day02 extends SimpleCommonPuzzle[Seq[Report], Int, Int] {

  override def parse(resource: String): Seq[Report] =
    readResource(resource)
      .map(line => line.split(" ").map(_.toInt))
      .map(Report(_))

  override def part1(input: Seq[Report]): Int = input.count(_.isSafe)

  override def part2(input: Seq[Report]): Int = input.map(_.dampened).count(_.exists(_.isSafe))
}
