package com.lmat.adventofcode.year2022

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

object Day06 extends SimpleCommonPuzzle[String, Int, Int] {

  override def parse(resource: String): String =
    readResource(resource).head

  override def part1(input: String): Int = {
    val startOfPacketMarkerLength = 4
    input.sliding(startOfPacketMarkerLength).zipWithIndex.filter(t => t._1.toSet.size == t._1.length).next()._2 + startOfPacketMarkerLength
  }

  override def part2(input: String): Int = {
    val startOfMessageMarkerLength = 14
    input.sliding(startOfMessageMarkerLength).zipWithIndex.filter(t => t._1.toSet.size == t._1.length).next()._2 + startOfMessageMarkerLength
  }
}
