package com.lmat.adventofcode.year2017

import com.lmat.adventofcode.year2017.Day06Definitions.Result
import com.lmat.adventofcode.CommonPuzzle
import com.lmat.util.Files.readResource

import scala.annotation.tailrec

object Day06Definitions {
  case class Result(count: Int, size: Int)
}

object Day06 extends CommonPuzzle[Vector[Int], Result, Int, Int] {
  type Blocks = Vector[Int]

  override def parse(resource: String): Blocks =
    readResource(resource).head.split("\\s+").map(_.toInt).toVector

  override def preProcess(blocks: Blocks): Result = countRedistributionCycles(blocks)

  override def part1(result: Result): Int = result.count

  override def part2(result: Result): Int = result.size

  def countRedistributionCycles(blocks: Blocks): Result = {
    @tailrec
    def countCycles(seen: Vector[Blocks], current: Blocks, count: Int): Result =
      if (seen.contains(current)) Result(count, count - seen.indexOf(current))
      else countCycles(seen :+ current, redistribute(current), count + 1)

    countCycles(Vector(), blocks, 0)
  }

  /**
    * To avoid simulating the redistribution we calculate the end state for each bank
    * We check how many full rounds the redistribution does and who gets the rest
    */
  def redistribute(blocks: Blocks): Blocks = {
    val indexed = blocks.zipWithIndex
    val (max, maxIndex) = indexed.maxBy(_._1)
    val size = indexed.size

    val rounds = max / size
    val mod = max % size
    val modIndices = (maxIndex + 1 to maxIndex + mod).map(_ % size)

    indexed.map{
      case (_, i) if i == maxIndex && modIndices.contains(i) => rounds + 1
      case (_, i) if i == maxIndex                           => rounds
      case (v, i) if modIndices.contains(i)                  => rounds + v + 1
      case (v, _)                                            => rounds + v
    }
  }
}
