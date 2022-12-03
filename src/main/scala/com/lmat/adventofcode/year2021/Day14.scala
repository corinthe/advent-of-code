package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2021.Day14Definitions.{Problem, Rule}
import com.lmat.util.Files.readResource

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day14Definitions {

  case class Problem(polymer: String, rules: Seq[Rule]) {

    var rMap: Map[(String, Int), Rule] = rules.map(r => (r.pair, 1) -> r).toMap

    var warmed = false

    def round(n: Int = 1): String = {
      @tailrec
      def recRound(n: Int, p: String): String = n match {
        case 0 => p
        case x if x >= 10 && warmed => recRound(n - 10, polymerise(p, 10))
        case _ => recRound(n - 1, polymerise(p))
      }
      recRound(n, polymer)
    }

    def prewarmCache(): Unit = {
      round(10)
        .sliding(12, 11)
        .foreach(s => rMap += ((s.head.toString + s.last.toString, 10) -> Rule(s.head.toString + s.last, s.tail.init)))

      warmed = true
    }

    def polymerise(p: String, steps: Int = 1): String =
      p.head + p
        .toSeq
        .sliding(2)
        .map(s => rMap(s.mkString, steps).execute())
        .map(s => s.tail)
        .mkString

    def solve1(n: Int = 1): Int = {
      val counts = round(n).groupMapReduce(identity)(_ => 1)(_ + _)
      counts.maxBy(_._2)._2 - counts.minBy(_._2)._2
    }

    def solve2(n: Int = 1): Int = {
      prewarmCache()
      solve1(n)
    }
  }

  case class Rule(pair: String, insertion: String) {
    def execute(): String = pair.head + insertion + pair.last
  }
}

object Day14 extends SimpleCommonPuzzle[Problem, Int, Int] {
  val PATTERN: Regex = """(.+) -> (.*)""".r

  override def parse(resource: String): Problem = {
    val lines = readResource(resource)
    val input = lines.head
    val rules = lines.drop(2).map {
      case PATTERN(p, i) => Rule(p, i)
      case _ => Rule("", "")
    }
    Problem(input, rules)
  }

  override def part1(input: Problem): Int = {
    input.solve1(10)
  }

  override def part2(input: Problem): Int = {
    input.solve2(10)
  }
}
