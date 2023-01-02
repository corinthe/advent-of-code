package com.lmat.adventofcode.year2022

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2022.Day15Definitions.{Point, Sensor}
import com.lmat.util.Files.readResource

import scala.util.matching.Regex

object Day15Definitions {

  case class Point(x: Int, y: Int) {

    def delta(dx: Int, dy: Int): Point = Point(x + dx, y + dy)

    def manhattanDistance(other: Point): Int = math.abs(other.x - x) + math.abs(other.y - y)

    def neighbours: Set[Point] = Set((1, 0), (-1, 0), (0, 1), (0, -1)).map(p => delta(p._1, p._2))

  }

  case class Sensor(p: Point, closestBeacon: Point) {

    val distance: Int = p.manhattanDistance(closestBeacon)

    def canReachLine(lineNumber: Int): Boolean = distance >= distanceToLine(lineNumber)

    def canReachPoint(other: Point) = distance <= p.manhattanDistance(other)

    def distanceToLine(lineNumber: Int): Int = p.manhattanDistance(Point(p.x, lineNumber))

    def pointsReachedOnLine(lineNumber: Int): Set[Point] = {
      if (!canReachLine(lineNumber)) Set()
      else {
        val verticalDistance = distanceToLine(lineNumber)
        val horizontalDistance = distance - verticalDistance
        ((p.x - horizontalDistance) to (p.x + horizontalDistance)).map(Point(_, lineNumber)).toSet
      }
    }
  }
}

object Day15 extends SimpleCommonPuzzle[Seq[Sensor], Int, Int] {

  def parseLine(s: String): Sensor = {
    val LINE_REGEX: Regex = """Sensor at x=(.+), y=(.+): closest beacon is at x=(.+), y=(.+)""".r
    s match {
      case LINE_REGEX(x1, y1, x2, y2) => Sensor(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
      case _ => throw new IllegalStateException()
    }
  }

  override def parse(resource: String): Seq[Sensor] =
    readResource(resource).map(parseLine)

  override def part1(input: Seq[Sensor]): Int =
    ((input map (_.pointsReachedOnLine(2000000))
      reduce (_ ++ _)
      diff input.map(_.p).toSet)
      diff input.map(_.closestBeacon).toSet)
      .size

  override def part2(input: Seq[Sensor]): Int = {
    val lowerBound = 0
    val upperBound = 4000000

    val test = Array.ofDim[Boolean](upperBound, upperBound)

    def tuningFrequency(p: Point): Int = (upperBound * p.x) + p.y

    input.foreach(s => {
      for {
        x <- math.max(s.p.x - s.distance, lowerBound) to math.min(s.p.x + s.distance, upperBound)
        y <- math.max(s.p.y - s.distance, lowerBound) to math.min(s.p.y + s.distance, upperBound)
      } test(x)(y) = true
    })

    (for {
      x <- test.indices
      y <- test(x).indices
      if test(x)(y)
    } yield tuningFrequency(Point(x, y))).head

  }
}
