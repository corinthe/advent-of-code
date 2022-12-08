package com.lmat.adventofcode.year2022

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource
import com.lmat.util.Matrix
import com.lmat.util.Matrix._
import com.lmat.util.Sequences.TakeUntilListWrapper


object Day08 extends SimpleCommonPuzzle[Matrix[Int], Int, Int] {

  override def parse(resource: String): Matrix[Int] =
    Matrix(readResource(resource)
      .toVector
      .map(_.toCharArray.map(d => d.toInt - '0'.toInt).toVector))

  override def part1(input: Matrix[Int]): Int = {
    val m = Matrix(input.rows)
    m
      .rows
      .zipWithIndex
      .flatMap(t => t._1.zipWithIndex.map(u => (u._1, t._2, u._2)))
      .count {
        case (value, row, col) if row == 0 || row == (m.height - 1) =>
          //if(row == 1 && col == 4) println("border 1")
          true
        case (value, row, col) if col == 0 || col == (m.width - 1) =>
          //if(row == 1 && col == 4) println("border 2")
          true
        case (value, row, col) if Matrix.ups(m, row, col).forall(_ < value) =>
          //if(row == 1 && col == 2) println("up")
          true
        case (value, row, col) if Matrix.downs(m, row, col).forall(_ < value) =>
          //if(row == 1 && col == 4) println("down")
          true
        case (value, row, col) if Matrix.lefts(m, row, col).forall(_ < value) =>
          //if(row == 1 && col == 4) println("left")
          true
        case (value, row, col) if Matrix.rights(m, row, col).forall(_ < value) =>
          //if(row == 1 && col == 4) println("right")
          true
        case (value, row, col) =>
          //if(row == 1 && col == 4) println("none")
          false
      }
  }


  override def part2(input: Matrix[Int]): Int = {
    val m = Matrix(input.rows)

    m
      .rows
      .zipWithIndex
      .flatMap(t => t._1.zipWithIndex.map(u => (u._1, t._2, u._2)))
      .map { case (value, row, col) =>
        Matrix.ups(m, row, col).reverse.takeUntil(_ < value).size *
          Matrix.downs(m, row, col).takeUntil(_ < value).size *
          Matrix.lefts(m, row, col).reverse.takeUntil(_ < value).size *
          Matrix.rights(m, row, col).takeUntil(_ < value).size
      }.max
  }
}
