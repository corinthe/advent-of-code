package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource
import com.lmat.util.{Maths, Matrix}

import scala.collection.immutable

object Day11 extends SimpleCommonPuzzle[Matrix[Char], Long, Long] {

  override def parse(resource: String): Matrix[Char] =
    Matrix(readResource(resource).toVector.map(_.toVector))

  private def expand(rows: Vector[Vector[Char]], size: Int = 2) =
    Matrix(rows.flatMap(row =>
      if (row.forall(c => c.equals('.'))) Seq.fill(size)(row).toVector
      else Vector(row)
    ))

  override def part1(input: Matrix[Char]): Long = {
    expand(expand(input.rows).columns)
      .rows
      .zipWithIndex
      .flatMap(r => r._1.zipWithIndex.filter(cell => cell._1 == '#').map(cell => (r._2.toLong, cell._2.toLong)))
      .combinations(2).map(combination => Maths.manhattan(combination.head, combination.last))
      .sum

  }

  private def expandEfficient(rows: Vector[Vector[(Char, Long, Long)]], increment: Int = 1, isRow: Boolean = true) = {
    var curr = 0
    Matrix(rows.map(row => {
      if (row.forall(c => c._1.equals('.'))) {
        curr += increment
      }
      if(isRow) row.map(c => (c._1, c._2 + curr, c._3)) else row.map(c => (c._1, c._2, c._3  + curr))
    }
    ))
  }

  override def part2(input: Matrix[Char]): Long = {
    val withIndex = Matrix(input.rows.zipWithIndex.map(row => row._1.zipWithIndex.map(cell => (cell._1, row._2.toLong, cell._2.toLong))))

    expandEfficient(expandEfficient(withIndex.rows, 1000000 - 1).columns, 1000000 - 1, isRow = false)
      .rows
      .flatMap(row => row.filter(cell => cell._1 == '#').map(cell => (cell._2, cell._3)))
      .combinations(2)
      .map(combination => Maths.manhattan(combination.head, combination.last))
      .sum
  }
}
