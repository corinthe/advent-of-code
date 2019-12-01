package com.lmat.adventofcode.year2019

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.annotation.tailrec
import scala.util.Try

object Day01 extends SimpleCommonPuzzle[Seq[Int], Int, Int] {
  override def parse(resource: String): Seq[Int] =
    readResource(resource).flatMap(row => Try(row.toInt).toOption)

  override def part1(masses: Seq[Int]): Int =
    masses.map(mass => fuel(mass)).sum

  override def part2(masses: Seq[Int]): Int =
    masses.map(mass => totalMass(mass)).sum

  def totalMass(initialMass: Int): Int = {
    @tailrec
    def iterate(remaining: Int, total: Int) : Int = remaining match {
      case remaining if remaining <= 0 => total
      case remaining => iterate(fuel(remaining), remaining + total)
    }

    iterate(fuel(initialMass), 0)
  }

  def fuel(mass: Int): Int = (mass / 3) - 2

}
