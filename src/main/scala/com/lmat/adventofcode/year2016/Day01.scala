package com.lmat.adventofcode.year2016

import com.lmat.adventofcode.SimpleMultiPuzzle
import com.lmat.adventofcode.year2016.Day01Definitions._
import com.lmat.util.Files.readResource

import scala.util.matching.Regex

object Day01Definitions {

  type Direction = String

  trait Direction2

  case object Up extends Direction2

  case object Down extends Direction2

  case object Left extends Direction2

  case object Right extends Direction2

  trait Command

  case object GoStraight extends Command

  case object TurnLeft extends Command

  case object TurnRight extends Command


  case class Mouvement(direction: String, distance: Int)

  case class Position(direction: Direction, distanceH: Int, distanceV: Int) {

    def move(mouvement: Mouvement): Position = direction match {
      case d if d.equalsIgnoreCase("U") && mouvement.direction.equalsIgnoreCase("L") => Position(mouvement.direction, distanceH - mouvement.distance, distanceV)
      case d if d.equalsIgnoreCase("U") && mouvement.direction.equalsIgnoreCase("R") => Position(mouvement.direction, distanceH + mouvement.distance, distanceV)
      case d if d.equalsIgnoreCase("L") && mouvement.direction.equalsIgnoreCase("L") => Position("D", distanceH, distanceV - mouvement.distance)
      case d if d.equalsIgnoreCase("L") && mouvement.direction.equalsIgnoreCase("R") => Position("U", distanceH, distanceV + mouvement.distance)
      case d if d.equalsIgnoreCase("R") && mouvement.direction.equalsIgnoreCase("L") => Position("U", distanceH, distanceV + mouvement.distance)
      case d if d.equalsIgnoreCase("R") && mouvement.direction.equalsIgnoreCase("R") => Position("D", distanceH, distanceV - mouvement.distance)
      case d if d.equalsIgnoreCase("D") && mouvement.direction.equalsIgnoreCase("L") => Position("R", distanceH + mouvement.distance, distanceV)
      case d if d.equalsIgnoreCase("D") && mouvement.direction.equalsIgnoreCase("R") => Position("L", distanceH - mouvement.distance, distanceV)
      case _ => throw new IllegalStateException(mouvement.toString + " " + this.toString)
    }

    def distanceFromOrigin(): Int = distanceV.abs + distanceH.abs
  }

  case class Location(h: Int, v: Int)

  case object Problem {
    var locations: Seq[Location] = Seq(Location(0, 0))
    private var currentDirection: Direction2 = Up

    def move(c: Command): Unit = c match {
      case GoStraight if currentDirection == Up => locations :+ Some(Location(locations.last.h, locations.last.v + 1))
      case GoStraight if currentDirection == Down => locations :+ Some(Location(locations.last.h, locations.last.v - 1))
      case GoStraight if currentDirection == Left => locations :+ Some(Location(locations.last.h - 1, locations.last.v))
      case GoStraight if currentDirection == Right => locations :+ Some(Location(locations.last.h + 1, locations.last.v))
      case TurnLeft if currentDirection == Up =>
        currentDirection = Left
        None
      case TurnLeft if currentDirection == Down =>
        currentDirection = Right
        None
      case TurnLeft if currentDirection == Left =>
        currentDirection = Down
        None
      case TurnLeft if currentDirection == Right =>
        currentDirection = Up
        None
      case TurnRight if currentDirection == Up =>
        currentDirection = Right
        None
      case TurnRight if currentDirection == Down =>
        currentDirection = Left
        None
      case TurnRight if currentDirection == Left =>
        currentDirection = Up
        None
      case TurnRight if currentDirection == Right =>
        currentDirection = Down
        None
      case _ => throw new IllegalStateException()
    }
  }

}

object Day01 extends SimpleMultiPuzzle[Seq[Mouvement], Int, Seq[Command], Int] {

  val PATTERN: Regex = """(.)(.+)""".r

  private def positions(input: Seq[Mouvement]) = input.scanLeft(Position("U", 0, 0))(_.move(_)).map(p => (p.distanceH, p.distanceV))

  override def parse1(resource: String): Seq[Mouvement] =
    readResource(resource)
      .flatMap(s => s.split(',').toSeq)
      .map(s => s.trim)
      .map {
        case PATTERN(d, m) => Mouvement(d, m.toInt)
      }


  override def part1(input: Seq[Mouvement]): Int = {
    val r = positions(input).last
    r._1.abs + r._2.abs
  }

  override def parse2(resource: String): Seq[Command] =
    readResource(resource)
      .flatMap(s => s.split(',').toSeq)
      .map(s => s.trim)
      .flatMap {
        case PATTERN(d, m) if d.equalsIgnoreCase("L") => TurnLeft +: Seq.fill(m.toInt)(GoStraight)
        case PATTERN(d, m) if d.equalsIgnoreCase("R") => TurnRight +: Seq.fill(m.toInt)(GoStraight)
      }

  override def part2(input: Seq[Command]): Int = {
    val p = Problem
    input.foreach(i => p.move(i))

    0
  }
}
