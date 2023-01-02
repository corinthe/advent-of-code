package com.lmat.adventofcode.year2022

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2022.Day18Definitions.Cube
import com.lmat.util.Files.readResource

import scala.util.matching.Regex

object Day18Definitions {

  case class Cube(x: Int, y: Int, z: Int) {
    def isAdjacent(other: Cube): Boolean = (math.abs(x - other.x) <= 1 && y == other.y && z == other.z) ||
      (math.abs(y - other.y) <= 1 && x == other.x && z == other.z) ||
      (math.abs(z - other.z) <= 1 && x == other.x && y == other.y)
  }
}

object Day18 extends SimpleCommonPuzzle[Seq[Cube], Int, Int] {

  val CUBE_REGEX: Regex = """(.*),(.*),(.*)""".r

  override def parse(resource: String): Seq[Cube] =
    readResource(resource).map {
      case CUBE_REGEX(x, y, z) => Cube(x.toInt, y.toInt, z.toInt)
    }

  override def part1(input: Seq[Cube]): Int =
    (input.length * 6) - (2 * input.combinations(2).count(comb => comb.head.isAdjacent(comb.last)))

  override def part2(input: Seq[Cube]): Int = 0
}
