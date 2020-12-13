package com.lmat.adventofcode.year2020

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2020.Day12Definitions._
import com.lmat.util.Files.readResource

import scala.annotation.tailrec

object Day12Definitions {

  trait Action

  trait Direction

  case class Position(x: Long, y: Long, facing: Direction) {

    def manhattan(): Long = math.abs(x) + math.abs(y)

    def execute(i: Instruction): Position = i match {
      case Instruction(MoveNorth, arg) => Position(x + arg, y, facing)
      case Instruction(MoveSouth, arg) => Position(x - arg, y, facing)
      case Instruction(MoveEast, arg) => Position(x, y + arg, facing)
      case Instruction(MoveWest, arg) => Position(x, y - arg, facing)
      case Instruction(TurnLeft, arg) => Position(x, y, turn(facing, -arg))
      case Instruction(TurnRight, arg) => Position(x, y, turn(facing, arg))
      case Instruction(MoveForward, arg) => facing match {
        case North => execute(Instruction(MoveNorth, arg))
        case South => execute(Instruction(MoveSouth, arg))
        case East => execute(Instruction(MoveEast, arg))
        case West => execute(Instruction(MoveWest, arg))
      }
      case _ => throw new IllegalStateException()
    }

    @tailrec
    private def turn(from: Direction, by: Int): Direction = {
      (from, by) match {
        case (dir, left) if left < 0 => dir match {
          case North => turn(West, left + 90)
          case West => turn(South, left + 90)
          case South => turn(East, left + 90)
          case East => turn(North, left + 90)
        }
        case (dir, right) if right > 0 => dir match {
          case North => turn(East, right - 90)
          case East => turn(South, right - 90)
          case South => turn(West, right - 90)
          case West => turn(North, right - 90)
        }
        case (dir, _) if by == 0 => dir
      }
    }

  }

  case class Instruction(a: Action, value: Int)

  case object MoveNorth extends Action

  case object MoveSouth extends Action

  case object MoveEast extends Action

  case object MoveWest extends Action

  case object TurnLeft extends Action

  case object TurnRight extends Action

  case object MoveForward extends Action

  case object North extends Direction

  case object South extends Direction

  case object East extends Direction

  case object West extends Direction

}

object Day12 extends SimpleCommonPuzzle[Seq[Instruction], Long, Int] {
  val PATTERN = """(.)(.+)""".r

  override def parse(resource: String): Seq[Instruction] =
    readResource(resource).map(parseLine)

  def parseLine(line: String): Instruction = line match {
    case PATTERN("N", arg) => Instruction(MoveNorth, arg.toInt)
    case PATTERN("S", arg) => Instruction(MoveSouth, arg.toInt)
    case PATTERN("E", arg) => Instruction(MoveEast, arg.toInt)
    case PATTERN("W", arg) => Instruction(MoveWest, arg.toInt)
    case PATTERN("L", arg) => Instruction(TurnLeft, arg.toInt)
    case PATTERN("R", arg) => Instruction(TurnRight, arg.toInt)
    case PATTERN("F", arg) => Instruction(MoveForward, arg.toInt)
  }

  override def part1(input: Seq[Instruction]): Long = {
    val start = Position(0, 0, East)

    val response = input.tapEach(println).foldLeft(start)(_.execute(_))

    println(response)

    response.manhattan()
  }

  override def part2(input: Seq[Instruction]): Int = 0
}
