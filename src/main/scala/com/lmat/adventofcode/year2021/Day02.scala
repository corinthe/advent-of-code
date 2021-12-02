package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2021.Day02Definitions.{ComplicatedPosition, Down, Forward, Instruction, Position, Up}
import com.lmat.util.Files.readResource

import scala.util.matching.Regex

object Day02Definitions {

  trait Direction

  case class Instruction(direction: Direction, distance: Int) {

  }

  case class Position(vertical: Int = 0, horizontal: Int = 0) {

    def move(i: Instruction): Position = i match {
      case Instruction(Forward, d) => Position(vertical, horizontal + d)
      case Instruction(Up, d) => Position(vertical - d, horizontal)
      case Instruction(Down, d) => Position(vertical + d, horizontal)
      case _ => throw new IllegalStateException()
    }

    def score(): Int = vertical * horizontal
  }

  case class ComplicatedPosition(vertical: Int = 0, horizontal: Int = 0, aim: Int = 0) {

    def move(i: Instruction): ComplicatedPosition = i match {
      case Instruction(Forward, d) => ComplicatedPosition(vertical + (aim * d), horizontal + d, aim)
      case Instruction(Up, d) => ComplicatedPosition(vertical, horizontal, aim - d)
      case Instruction(Down, d) => ComplicatedPosition(vertical, horizontal, aim + d)
      case _ => throw new IllegalStateException()
    }

    def score(): Int = vertical * horizontal
  }

  case object Forward extends Direction

  case object Up extends Direction

  case object Down extends Direction
}

object Day02 extends SimpleCommonPuzzle[Seq[Instruction], Int, Int] {

  val PATTERN: Regex = """(.+) (.+)""".r

  override def parse(resource: String): Seq[Instruction] =
    readResource(resource).map(parseLine)

  def parseLine(line: String): Instruction = line match {
    case PATTERN("forward", x) => Instruction(Forward, x.toInt)
    case PATTERN("up", x) => Instruction(Up, x.toInt)
    case PATTERN("down", x) => Instruction(Down, x.toInt)
  }

  override def part1(instructions: Seq[Instruction]): Int =
    instructions.foldLeft(Position())((acc, curr) => acc.move(curr)).score()


  override def part2(instructions: Seq[Instruction]): Int =
    instructions.foldLeft(ComplicatedPosition())((acc, curr) => acc.move(curr)).score()

}
