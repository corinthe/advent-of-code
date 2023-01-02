package com.lmat.adventofcode.year2022

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource
import com.lmat.util.Matrix

object Day14Definitions {

  case class Point(x: Int, y: Int) {
  }

  trait Material

  case object Sand extends Material

  case object SandGenerator extends Material

  case object Rock extends Material

  case object Void extends Material

  case class Cave(m: Matrix[Material] = Matrix.ofSize(10000, Void)) {

    def solve1(): Int = 0
  }
}

object Day14 extends SimpleCommonPuzzle[Seq[String], Int, Int] {

  override def parse(resource: String): Seq[String] =
    readResource(resource)

  override def part1(input: Seq[String]): Int = ???

  override def part2(input: Seq[String]): Int = ???
}
