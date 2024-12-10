package com.lmat.adventofcode.year2024

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2024.Day10Definitions.TopologicalMap
import com.lmat.util.Files.readResource
import com.lmat.util.Matrix

object Day10Definitions {
  case class Trail(coords: Seq[(Int, Int)]) {
    override def toString: String = coords.mkString
  }

  case class TopologicalMap(map: Matrix[Int]) {
    override def toString: String = map.toString

    def findTrails(): Seq[Trail] = {
      def findTrail(current: (Int, Int), expectedValue: Int, path: Seq[(Int, Int)]): Seq[Seq[(Int, Int)]] = {
        if (expectedValue > 9) {
          Seq(path)
        } else {
          val neighbors = map.getNeighbors(current)
          neighbors.flatMap { next =>
            if (map.rows(next._1)(next._2) == expectedValue) {
              findTrail(next, expectedValue + 1, path :+ next)
            } else {
              Seq.empty
            }
          }
        }
      }

      map.findAllPositions(0).flatMap(start => findTrail(start, 1, Seq(start))).map(Trail(_))
    }

  }
}

object Day10 extends SimpleCommonPuzzle[TopologicalMap, Int, Int] {

  override def parse(resource: String): TopologicalMap =
    TopologicalMap(Matrix(readResource(resource).map(s => s.map(_.asDigit).toVector).toVector))

  override def part1(input: TopologicalMap): Int = {
    input
      .findTrails()
      .groupMap(_.coords.head)(_.coords.last)
      .foldLeft(0){ case (x, y) => x + y._2.distinct.length}

  }

  override def part2(input: TopologicalMap): Int = input.findTrails().length

}
