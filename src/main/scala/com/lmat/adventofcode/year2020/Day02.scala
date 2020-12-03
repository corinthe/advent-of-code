package com.lmat.adventofcode.year2020

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2020.Day02Definitions.Row
import com.lmat.util.Files.readResource

import scala.util.Try

object Day02Definitions {

  case class Row(x: Int, y: Int, char: Char, password: String)

}

object Day02 extends SimpleCommonPuzzle[Seq[Option[Row]], Int, Int] {
  override def parse(resource: String): Seq[Option[Row]] =
    readResource(resource).map(row => parseRow(row))

  def parseRow(row: String): Option[Row] = {
    val pattern = "(.*?)-(.*?) (.?): (.*?)".r

    row match {
      case pattern(x, y, char, passwords) => (for {
        minOcc <- Try(x.toInt)
        maxOcc <- Try(y.toInt)
        requiredChar <- Try(char.head)
        password <- Try(passwords)
      } yield Row(minOcc, maxOcc, requiredChar, password)).toOption
      case _ => None
    }
  }

  override def part1(input: Seq[Option[Row]]): Int = {
    def passwordMatchPolicy(row: Row): Boolean =
      (row.x to row.y) contains row.password.count(_ == row.char)

    input.count(row => passwordMatchPolicy(row.get))
  }

  override def part2(input: Seq[Option[Row]]): Int = {
    def passwordMatchPolicy(row: Row): Boolean =
      row.password.charAt(row.x - 1) == row.char ^ row.password.charAt(row.y - 1) == row.char

    input.count(row => passwordMatchPolicy(row.get))
  }
}
