package com.lmat.adventofcode.year2020

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2020.Day08Definitions.{Command, Problem}
import com.lmat.util.Files.readResource

import scala.annotation.tailrec

object Day08Definitions {

  case class Command(op: Operation, arg: Int)

  type Operation = String

  case class Problem(commands: Array[Command]) {

    def targetIndex(): Int = commands.length

    def canSwitch(command: Command): Boolean = command.op != "acc"

    def switchCommand(command: Command): Command = command match {
      case Command("acc", arg) => Command("acc", arg)
      case Command("nop", arg) => Command("jmp", arg)
      case Command("jmp", arg) => Command("nop", arg)
      case _ => throw new IllegalStateException("Illegal instruction")
    }

    def switchIndex(i: Int): Unit = {
      this.commands(i) = switchCommand(commands(i))
    }

    def completes(): Boolean = {
      @tailrec
      def completesRec(currIndex: Int, checked: Set[Int], commands: Seq[Command]): Boolean =
        if (checked.contains(currIndex)) false
        else if (currIndex == targetIndex()) true
        else

          commands(currIndex) match {
            case Command("nop", _) => completesRec(currIndex + 1, checked + currIndex, commands)
            case Command("jmp", arg) => completesRec(currIndex + arg, checked + currIndex, commands)
            case Command("acc", _) => completesRec(currIndex + 1, checked + currIndex, commands)
            case _ => throw new IllegalStateException("Illegal instruction")
          }

      completesRec(0, Set(), commands)
    }

    def acc(): Int = {
      @tailrec
      def accRec(currIndex: Int, checked: Set[Int], visited: Seq[Int], commands: Seq[Command]): Seq[Int] =
        if (currIndex == targetIndex()) visited else

          commands(currIndex) match {
            case Command("nop", _) => accRec(currIndex + 1, checked + currIndex, visited, commands)
            case Command("jmp", arg) => accRec(currIndex + arg, checked + currIndex, visited, commands)
            case Command("acc", arg) => accRec(currIndex + 1, checked + currIndex, visited :+ arg, commands)
            case _ => throw new IllegalStateException("Illegal instruction")
          }
      accRec(0, Set(), Seq(), commands).sum
    }

    def debug(): Int =
      LazyList
        .from(0)
        .map(i => {
          val p = Problem(commands.clone())
          p.switchIndex(i)
          p
        })
        .find(_.completes())
        .map(_.acc())
        .sum
  }

}

object Day08 extends SimpleCommonPuzzle[Seq[Command], Int, Int] {

  val PATTERN = """(^.+) (.+)""".r

  def parseLine(input: String): Command = input match {
    case PATTERN(op, arg) => Command(op, arg.toInt)
  }

  override def parse(resource: String): Seq[Command] =
    readResource(resource).map(parseLine)

  override def part1(input: Seq[Command]): Int = rec1(0, Set(), Seq(), input)

  @tailrec
  def rec1(currIndex: Int, checked: Set[Int], visited: Seq[Int], commands: Seq[Command]): Int =
    if (checked.contains(currIndex)) visited.sum else

      commands(currIndex) match {
        case Command("nop", _) => rec1(currIndex + 1, checked + currIndex, visited, commands)
        case Command("jmp", arg) => rec1(currIndex + arg, checked + currIndex, visited, commands)
        case Command("acc", arg) => rec1(currIndex + 1, checked + currIndex, visited :+ arg, commands)
        case _ => throw new IllegalStateException("Illegal instruction")
      }

  override def part2(input: Seq[Command]): Int =
    Problem(input.toArray).debug()
}
