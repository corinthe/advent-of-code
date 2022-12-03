package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.util.matching.Regex

object Day21 extends SimpleCommonPuzzle[(Int, Int), Int, Int] {

  override def parse(resource: String): (Int, Int) = {
    val pos1 = readResource(resource).head.last.toString.toInt
    val pos2 = readResource(resource).last.last.toString.toInt
    (pos1, pos2)
  }

  override def part1(input: (Int, Int)): Int = {
    val target: Int = 1000
    val boardSize = 10
    var score1 = input._1
    var score2 = input._2
    val rolls = LazyList.continually( 1 to 100).flatten
      .sliding(3, 3)
      .map(_.sum)
      .zipWithIndex
      .map { case (sum, i) => if(i % 2 == 1) score1  += ((score1 + sum) % boardSize) else score2 += ((score2 + sum) % boardSize)}
      .takeWhile(_ => score1 < target && score2 < target)
      .size
    println(rolls)
    println(score1.min(score2))
    score2.min(score1) * rolls
  }

  override def part2(input: (Int, Int)): Int = 0
}
