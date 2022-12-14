package com.lmat.adventofcode.year2022

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.annotation.tailrec


object Day13 extends SimpleCommonPuzzle[Seq[String], Int, Int] {


  @tailrec
  def rightOrder(s1: String, s2: String): Boolean = (s1.head, s2.head) match {
    case (a, b) if a == b => rightOrder(s1.tail, s2.tail)
    case (']', _) => true
    case (_, ']') => false
    case ('[', digit) => rightOrder(s1, "[" + digit + "]" + s2.tail)
    case (digit, '[') => rightOrder("[" + digit + "]" + s1.tail, s2)
    case (a, b) => a < b
  }


  override def parse(resource: String): Seq[String] =
    readResource(resource)
      .filter(s => s.nonEmpty)
      .map(_.replace("10", "X"))

  override def part1(input: Seq[String]): Int =
    input.grouped(2).zipWithIndex.filter { case (packets, _) => rightOrder(packets.head, packets.last) }.map(_._2 + 1).sum


  override def part2(input: Seq[String]): Int = {
    val dividers = Seq("[[2]]", "[[6]]")
    val sorted = (input ++ dividers).sortWith(rightOrder)
    dividers.map(sorted.indexOf(_) + 1).product

  }
}
