package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2021.Day04Definitions.BingoBoard
import com.lmat.util.Files.readResource
import com.lmat.util.Matrix

import scala.util.matching.Regex

object Day04Definitions {

  case class BingoBoard(m: Seq[Matrix[Int]], draws: Seq[Int]) {

  }
}

object Day04 extends SimpleCommonPuzzle[Seq[String], Int, Int] {

  override def parse(resource: String): Seq[String] =
    readResource(resource)

  def parseProblem(lines: Seq[String]): BingoBoard = {
    val PATTERN: Regex = """(\d+) (\d+) (\d+) (\d+) (\d+)""".r

    val draws =  lines.head.split(",").map(_.toInt)

    null
  }

  override def part1(input: Seq[String]): Int = 0

  override def part2(input: Seq[String]): Int = 0
}
