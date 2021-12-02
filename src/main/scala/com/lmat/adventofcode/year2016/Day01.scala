package com.lmat.adventofcode.year2016

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2016.Day01Definitions.{Mouvement, Position}
import com.lmat.util.Files.readResource

import scala.util.matching.Regex

object Day01Definitions {

  type Direction = String

  case class Mouvement(direction: String, distance: Int)

  case class Position(direction: Direction, distanceH: Int, distanceV: Int) {

    def move(mouvement: Mouvement): Position = direction match {
      case d if d.equalsIgnoreCase("U") && mouvement.direction.equalsIgnoreCase( "L") => Position(mouvement.direction, distanceH - mouvement.distance, distanceV)
      case d if d.equalsIgnoreCase("U") && mouvement.direction.equalsIgnoreCase( "R") => Position(mouvement.direction, distanceH + mouvement.distance, distanceV)
      case d if d.equalsIgnoreCase("L") && mouvement.direction.equalsIgnoreCase( "L") => Position("D", distanceH, distanceV - mouvement.distance)
      case d if d.equalsIgnoreCase("L") && mouvement.direction.equalsIgnoreCase( "R") => Position("U", distanceH, distanceV + mouvement.distance)
      case d if d.equalsIgnoreCase("R") && mouvement.direction.equalsIgnoreCase( "L") => Position("U", distanceH, distanceV + mouvement.distance)
      case d if d.equalsIgnoreCase("R") && mouvement.direction.equalsIgnoreCase( "R") => Position("D", distanceH, distanceV - mouvement.distance)
      case d if d.equalsIgnoreCase("D") && mouvement.direction.equalsIgnoreCase( "L") => Position("R", distanceH + mouvement.distance, distanceV)
      case d if d.equalsIgnoreCase("D") && mouvement.direction.equalsIgnoreCase( "R") => Position("L", distanceH - mouvement.distance, distanceV)
      case _ => throw new IllegalStateException(mouvement.toString + " " + this.toString)
    }

    def distanceFromOrigin(): Int = distanceV.abs + distanceH.abs
  }

  case class Location(h: Int, v: Int)

}

object Day01 extends SimpleCommonPuzzle[Seq[Mouvement], Int, Int] {

  val PATTERN: Regex = """(.)(.+)""".r

  override def parse(resource: String): Seq[Mouvement] =
    readResource(resource)
      .flatMap(s => s.split(',').toSeq)
      .map(s => s.trim)
      .map {
        case PATTERN(d,m) => Mouvement(d, m.toInt)
      }

  override def part1(input: Seq[Mouvement]): Int = {
    input.foldLeft(Position("U", 0, 0))((acc, curr) => acc.move(curr)).distanceFromOrigin()
  }

  override def part2(input: Seq[Mouvement]): Int = 0
}
