package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import java.util.IntSummaryStatistics

object Day07 extends SimpleCommonPuzzle[Seq[Int], Double, Int] {

  override def parse(resource: String): Seq[Int] =
    readResource(resource).head.split(",").map(_.toInt)

  override def part1(crabs: Seq[Int]): Double = {
    val sorted = crabs.sorted
    val median = sorted(crabs.length/2)
    crabs.map(x => Math.abs(x - median)).sum
  }

  override def part2(crabs: Seq[Int]): Int = {
    val sorted = crabs.sorted
    val median = sorted(crabs.length/2)
    println(median)
    val average = crabs.sum.toDouble / crabs.length
    println(average)
    val target =  Math.ceil(average)
    println(target)
    crabs.map(x => cost(Math.abs(x - target.toInt))).sum
  }

  def cost(distance: Int): Int = LazyList.from(1).take(distance).sum
}
