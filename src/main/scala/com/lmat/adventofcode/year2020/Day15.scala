package com.lmat.adventofcode.year2020

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2020.Day15Definitions.{Numero, Position, Problem}
import com.lmat.util.Files.readResource

import scala.annotation.tailrec
import scala.collection.mutable

object Day15Definitions {
  type Position = Long
  type Numero = Long

  case class Problem(input: Seq[Numero], limit: Long) {
    val latestKnowPositions: mutable.Map[Numero, Position] = mutable.HashMap()
    input.dropRight(1)
      .zipWithIndex
      .foreach { case (value, index) => latestKnowPositions(value) = index + 1 }

    def solve(): Numero = {
      @tailrec
      def computeNext(currentValue: Numero, currPos: Position): Numero = currPos match {
        case pos if pos == limit => currentValue
        case _ =>
          val nextValue = currPos - latestKnowPositions.getOrElse(currentValue, currPos)
          latestKnowPositions(currentValue) = currPos
          computeNext(nextValue, currPos + 1)
      }
      computeNext(input.last, input.length)
    }
  }

}

object Day15 extends SimpleCommonPuzzle[Seq[Numero], Numero, Numero] {

  override def parse(resource: String): Seq[Numero] =
    readResource(resource).head.split(",").map(BigInt(_).toLong).toSeq


  override def part1(input: Seq[Numero]): Numero = {
    Problem(input, 2020).solve()
  }

  override def part2(input: Seq[Numero]): Numero = {
    Problem(input, 30000000).solve()
  }
}
