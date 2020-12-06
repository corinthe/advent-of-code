package com.lmat.adventofcode.year2020

import com.lmat.adventofcode.SimpleCommonPuzzle

import scala.io.Source

object Day06 extends SimpleCommonPuzzle[Seq[String], Int, Int] {

  val END_OF_LINE: String = "\r\n"

  override def parse(resource: String): Seq[String] =
    Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(resource))
      .mkString
      .split(END_OF_LINE + END_OF_LINE)
      .toSeq

  override def part1(groups: Seq[String]): Int =
    groups.map {
      _
        .replace(END_OF_LINE, "")
        .toSet
        .size
    }.sum

  override def part2(groups: Seq[String]): Int =
    groups.map {
      _
        .split(END_OF_LINE)
        .map(_.toSet)
        .reduce(_.intersect(_))
        .size
    }.sum
}
