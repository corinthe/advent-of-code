package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource
import com.lmat.util.Matrix

import scala.util.Try

object Day09 extends SimpleCommonPuzzle[Matrix[Int], Int, Int] {

  override def parse(resource: String): Matrix[Int] = Matrix(
    readResource(resource)
      .map(_.split("").map(_.toInt).toVector)
      .toVector)

  override def part1(input: Matrix[Int]): Int = {
    def getNeighbours(h: Int, v: Int): Seq[Int] = Seq(
      Try(input.rows(h)(v - 1)),
      Try(input.rows(h - 1)(v)),
      Try(input.rows(h)(v + 1)),
      Try(input.rows(h + 1)(v))
    ).flatMap(_.toOption)

    def isLowPoint(h: Int, v: Int, c: Int): Boolean = getNeighbours(h,v).forall(_ > c)

    val lowpoints = (for {
      rowIndex <- input.rows.indices
      colIndex <- input.rows(rowIndex).indices
    } yield (rowIndex, colIndex, input.rows(rowIndex)(colIndex)))
      .filter(x => isLowPoint(x._1, x._2, x._3))

    lowpoints.map(_._3).sum + lowpoints.length
  }

  override def part2(input: Matrix[Int]): Int = 0
}
