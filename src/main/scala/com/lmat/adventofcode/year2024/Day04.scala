package com.lmat.adventofcode.year2024

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource
import com.lmat.util.Matrix

object Day04 extends SimpleCommonPuzzle[Matrix[Char], Int, Int] {

  override def parse(resource: String): Matrix[Char] = {
    val m = Matrix(readResource(resource).map(_.toVector).toVector)
    m
  }

  override def part1(input: Matrix[Char]): Int = {
    val r = input.generateSequencesFromPositions(input.findAllPositions('X'), 4).map(_.mkString)
    r.count(_ == "XMAS")
  }

  override def part2(input: Matrix[Char]): Int = {
    val as = input.findAllPositions('A')
      .filter {
        case (x,y) if x == 0 || y == 0 || x == input.rows.length - 1 || y == input.rows.head.length - 1 => false
        case _ => true
      }
    as.map {
      a => (
        (input.rows(a._1 - 1)(a._2 + 1), input.rows(a._1 + 1)(a._2 - 1)),
        (input.rows(a._1 + 1)(a._2 + 1), input.rows(a._1 - 1)(a._2 - 1)))
    }.count {
      case (('M', 'S'), ('M', 'S')) => true
      case (('M', 'S'), ('S', 'M')) => true
      case (('S', 'M'), ('S', 'M')) => true
      case (('S', 'M'), ('M', 'S')) => true
      case _ => false
    }
  }
}
