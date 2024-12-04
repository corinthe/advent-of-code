package com.lmat.adventofcode.year2024

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2024.Day03Definitions.{Do, Dont, Mul, Op}
import com.lmat.util.Files.readResource

import scala.util.matching.Regex


object Day03Definitions {
  trait Op

  case class Mul(a: Int, b: Int) extends Op

  case object Do extends Op

  case object Dont extends Op
}

object Day03 extends SimpleCommonPuzzle[Seq[Op], Int, Int] {

  override def parse(resource: String): Seq[Op] = {
    val mulRegex = """mul\((\d{1,3}),(\d{1,3})\)""".r
    val doRegex = """do\(\)""".r
    val dontRegex = """don't\(\)""".r

    val ops: Seq[Op] = readResource(resource).flatMap { line =>
      val regexes = Seq(
        mulRegex -> ((m: Regex.Match) => Mul(m.group(1).toInt, m.group(2).toInt)),
        doRegex -> ((_: Regex.Match) => Do),
        dontRegex -> ((_: Regex.Match) => Dont)
      )

      // Find matches in order
      val matches = regexes.flatMap { case (regex, toOp) =>
        regex.findAllMatchIn(line).map(m => m.start -> toOp(m))
      }

      matches.sortBy(_._1).map(_._2)
    }
    ops
  }


  override def part1(input: Seq[Op]): Int = {
    input.collect { case m: Mul => m }.map(m => m.a * m.b).sum
  }

  override def part2(input: Seq[Op]): Int = {
    input.foldLeft((false, Seq.empty[Mul])) {
      case ((removalMode, acc), elem) => elem match {
        case Dont => (true, acc)
        case Do => (false, acc)
        case Mul(_, _) if removalMode => (true, acc)
        case Mul(_,_) => (removalMode, acc :+ elem.asInstanceOf[Mul])
      }
    }._2.map(m => m.a * m.b).sum
  }
}
