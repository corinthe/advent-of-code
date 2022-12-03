package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource
import com.lmat.util.Matrix

import java.lang.Integer.parseInt


object Day03 extends SimpleCommonPuzzle[Matrix[Int], Int, Int] {


  override def parse(resource: String): Matrix[Int] =
    Matrix(readResource(resource).map(parseLine).toVector)

  def parseLine(line: String): Vector[Int] = line.split("").map(_.toInt).toVector

  override def part1(report: Matrix[Int]): Int = {

    val gammaRate = parseInt(report.columns.map {_.groupBy(identity).maxBy(_._2.size)._1}.mkString, 2)
    val epsilonRate = parseInt(report.columns.map {_.groupBy(identity).minBy(_._2.size)._1}.mkString, 2)

    epsilonRate * gammaRate
  }


  override def part2(report: Matrix[Int]): Int = {
    val oxygenRate = parseInt(recMax(report.rows).head.mkString, 2)
    val co2Rate = parseInt(recMin(report.rows).head.mkString, 2)

    println(recMax(report.rows).head.mkString)
    println(recMin(report.rows).head.mkString)

    oxygenRate * co2Rate
  }


  def recMax(remainingRows: Vector[Vector[Int]], pos: Int = 0): Vector[Vector[Int]] = {
    if(remainingRows.length == 1) remainingRows
    else {
      val map = Matrix(remainingRows).columns(pos).groupBy(identity).maxBy(_._2.size)
        val maxOfCol = map._1
      if(pos == 2) println(map)
      recMax(remainingRows.filter(_(pos) == maxOfCol), pos + 1)
    }

  }

  def recMin(remainingRows: Vector[Vector[Int]], pos: Int = 0): Vector[Vector[Int]] = {
    if(remainingRows.length == 1) remainingRows
    else {
      val minOfCol = Matrix(remainingRows).columns(pos).groupBy(identity).minBy(_._2.size)._1
      recMax(remainingRows.filter(_(pos) == minOfCol), pos + 1)
    }

  }
}
