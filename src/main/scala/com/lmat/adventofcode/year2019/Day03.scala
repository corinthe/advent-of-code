package com.lmat.adventofcode.year2019

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.annotation.tailrec

object Day03 extends SimpleCommonPuzzle[Seq[Seq[String]], Int, Int] {

  override def parse(resource: String): Seq[Seq[String]] =
    readResource(resource).map(parseString)

  def parseString(str: String): Seq[String] =
    str.split(",").toIndexedSeq

  override def part1(wireOps: Seq[Seq[String]]): Int = {
    val traversed1 = buildList(wireOps(0))
    val traversed2 = buildList(wireOps(1))

    traversed1
      .distinct
      .intersect(traversed2.distinct)
      .filter(a => a != (0, 0))
      .map(x => manhattanDistance(x))
      .min
  }

  @tailrec
  def buildList(wireOps: Seq[String], traversedPositions: Seq[(Int, Int)] = Seq((0, 0))): Seq[(Int, Int)] = wireOps match {
    case first +: tail => buildList(tail, traversedPositions ++ listTraversed(first, traversedPositions.last))
    case Seq() => traversedPositions
  }

  def manhattanDistance(pointA: (Int, Int), pointB: (Int, Int) = (0, 0)): Int =
    Math.abs(pointA._1 - pointB._1) + Math.abs(pointB._2 - pointA._2)

  def listTraversed(op: String, startingPoint: (Int, Int)): Seq[(Int, Int)] = {
    val upRegex = "U([0-9]+)".r
    val downRegex = "D([0-9]+)".r
    val leftRegex = "L([0-9]+)".r
    val rightRegex = "R([0-9]+)".r

    op match {
      case upRegex(amount) => for (y <- (startingPoint._2 + 1) to (startingPoint._2 + amount.toInt)) yield (startingPoint._1, y)
      case downRegex(amount) => for (y <- (startingPoint._2 - 1) to (startingPoint._2 - amount.toInt) by -1) yield (startingPoint._1, y)
      case leftRegex(amount) => for (x <- (startingPoint._1 - 1) to (startingPoint._1 - amount.toInt) by -1) yield (x, startingPoint._2)
      case rightRegex(amount) => for (x <- (startingPoint._1 + 1) to startingPoint._1 + amount.toInt) yield (x, startingPoint._2)
    }
  }

  def findDistance(intersection: (Int, Int), possibles: Seq[((Int, Int), Int)]): Int =
    possibles.filter(p => p._1 == intersection).map(p => p._2).min

  override def part2(wireOps: Seq[Seq[String]]): Int = {

    val traversed1 = buildList(wireOps(0))
    val traversed2 = buildList(wireOps(1))

    val traversed1WithIndex = traversed1.zipWithIndex
    val traversed2WithIndex = traversed2.zipWithIndex

    traversed1
      .distinct
      .intersect(traversed2.distinct)
      .filter(a => a != (0, 0))
      .map(intersection => findDistance(intersection, traversed1WithIndex) + findDistance(intersection, traversed2WithIndex))
      .min


  }
}
