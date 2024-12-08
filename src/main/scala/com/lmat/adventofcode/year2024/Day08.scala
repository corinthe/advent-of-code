package com.lmat.adventofcode.year2024

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource
import com.lmat.util.Matrix

object Day08 extends SimpleCommonPuzzle[Matrix[Char], Int, Int] {

  override def parse(resource: String): Matrix[Char] = Matrix(readResource(resource).toVector.map(_.toVector))

  override def part1(input: Matrix[Char]): Int = {
    val antennas = input.rows.map(_.mkString).mkString.distinct.filterNot(_ == '.')
    val antennasPositions = antennas.map(input.findAllPositions)

    def findAntinode(a: (Int, Int), b: (Int, Int)): Seq[(Int, Int)] = Seq(
      (b._1 + b._1 - a._1, b._2 + b._2 - a._2), (a._1 - (b._1 - a._1), a._2 - (b._2 - a._2))
    )

    val antinodes = antennasPositions
      .flatMap(_.combinations(2).flatMap(x => findAntinode(x.head, x.last)))
      .distinct
      .filterNot(p => p._1 < 0 || p._2 < 0 || p._1 >= input.rows.length || p._2 >= input.columns.length)

    println(antinodes.mkString)

    antinodes.length
  }

  override def part2(input: Matrix[Char]): Int = {
    val antennas = input.rows.map(_.mkString).mkString.distinct.filterNot(_ == '.')
    val antennasPositions = antennas.map(input.findAllPositions)

    def findAntinode(a: (Int, Int), b: (Int, Int)): Seq[(Int, Int)] =  {
      val (x1, y1) = a
      val (x2, y2) = b

      // Validate points are within matrix bounds
      def isValidPoint(p: (Int, Int)): Boolean = {
        val (x, y) = p
        x >= 0 && x < input.rows.length && y >= 0 && y < input.columns.length
      }

      if(a == b) return Seq.empty

      val dx = x2 - x1
      val dy = y2 - y1

      // Find the greatest common divisor to get the smallest step
      def gcd(a: Int, b: Int): Int = if (b == 0) Math.abs(a) else gcd(b, a % b)
      val step = gcd(dx, dy)

      // Calculate unit step values
      val xStep = dx / step
      val yStep = dy / step

      val maxX = input.rows.length - 1
      val maxY = input.columns.length - 1

      def findBoundaryPoints(): (Int, Int) = {

        def stepsToLimit(x: Int, y: Int, dx: Int, dy: Int): Int = {
          if (dx == 0 && dy == 0) return 0
          val stepsX = if (dx != 0) {
            if (dx > 0) (maxX - x) / dx else -x / dx
          } else Int.MaxValue
          val stepsY = if (dy != 0) {
            if (dy > 0) (maxY - y) / dy else -y / dy
          } else Int.MaxValue
          Math.min(if (stepsX == Int.MaxValue) stepsY else stepsX,
            if (stepsY == Int.MaxValue) stepsX else stepsY)
        }

        // Find steps in both directions
        val stepsForward = stepsToLimit(x1, y1, xStep, yStep)
        val stepsBackward = stepsToLimit(x1, y1, -xStep, -yStep)

        (stepsBackward, stepsForward)
      }

      val (backSteps, forwardSteps) = findBoundaryPoints()

      // Generate all points on the line from boundary to boundary
      (-backSteps to forwardSteps).map { i =>
        (x1 + i * xStep, y1 + i * yStep)
      }.filter(isValidPoint)
    }

    val antinodes = antennasPositions
      .flatMap(_.combinations(2).flatMap(x => findAntinode(x.head, x.last)))
      .distinct

    antinodes.length
  }
}
