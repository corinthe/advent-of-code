package com.lmat.adventofcode.year2022

import com.lmat.adventofcode.{SimpleCommonPuzzle, SimpleMultiPuzzle}
import com.lmat.adventofcode.year2022.Day05Definitions.{Instruction, Problem}
import com.lmat.util.Files.readResource
import com.lmat.util.{Matrix, Strings}
import com.lmat.util.Matrix._

import scala.collection.mutable
import scala.util.matching.Regex

object Day05Definitions {

  case class Instruction(quantity: Int, from: Int, to: Int)

  case class Problem(piles: Array[mutable.Stack[Char]], instructions: Seq[Instruction]) {
    def solve1(): String = {

      instructions.foreach(followInstruction1)
      getTops
    }

    def solve2(): String = {
      instructions.foreach(followInstruction2)
      getTops
    }

    def followInstruction1(instruction: Instruction): Unit = {
      for (_ <- 1 to instruction.quantity) {
        piles(instruction.to - 1).push(piles(instruction.from - 1).pop())
      }
    }

    def followInstruction2(instruction: Instruction): Unit = {

      val popped = for (_ <- 1 to instruction.quantity)
        yield piles(instruction.from - 1).pop()

      piles(instruction.to - 1).pushAll(popped.reverse)
    }

    def getTops: String = piles.map(_.top).mkString
  }
}

object Day05 extends SimpleMultiPuzzle[Problem, String, Problem, String] {

  def parseInstruction(line: String): Instruction = {
    val INSTRUCTION_PATTERN: Regex = """move (\d*) from (\d*) to (\d*)""".r
    line match {
      case INSTRUCTION_PATTERN(quantity, from, to) => Instruction(quantity.toInt, from.toInt, to.toInt)
      case _ => throw new IllegalStateException("Illegal instruction")
    }
  }

  def parsePile(line: Vector[Char]): mutable.Stack[Char] = {
    mutable.Stack.from(line.filter(c => c >= 'A' && c <= 'Z').reverse)
  }

  override def parse1(resource: String): Problem = {
    val t = readResource(resource)
      .filter(_.nonEmpty)
      .partition(!_.contains("move"))

    Problem(
      rotateRight(Matrix(t._1.map(s => Strings.rightPad(s)('x', t._1.map(_.length).max)).map(_.toVector).toVector)).rows.map(parsePile).toArray.filter(_.nonEmpty),
      t._2.map(parseInstruction)
    )
  }


  override def part1(input: Problem): String = {
    input.solve1()
  }

  override def part2(input: Problem): String =
    input.solve2()

  override def parse2(resource: String): Problem = parse1(resource)
}
