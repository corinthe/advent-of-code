package com.lmat.adventofcode.year2020

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2020.Day07Definitions.{Color, Rule}
import com.lmat.util.Files.readResource

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day07Definitions {

  case class Rule(container: Color, content: (Int, Color))

  type Color = String

}

object Day07 extends SimpleCommonPuzzle[Seq[Rule], Int, Int] {

  val CONTAINER_PATTERN: Regex = """^(\w+ \w+)""".r
  val CONTENT_PATTERN: Regex = """(\d+) (\w+ \w+)""".r

  def parseRule(input: String): Seq[Rule] = {
    val container = CONTAINER_PATTERN.findFirstMatchIn(input).get.group(1)
    CONTENT_PATTERN.findAllMatchIn(input).toSeq.map(matched => Rule(container, (matched.group(1).toInt, matched.group(2))))

  }
  override def parse(resource: String): Seq[Rule] =
    readResource(resource).flatMap(parseRule)

  override def part1(rules: Seq[Rule]): Int = {
    rec1(Seq("shiny gold"), Set(), rules).size - 1
  }

  @tailrec
  def rec1(targets: Seq[Color], checked: Set[Color], rules: Seq[Rule]): Set[Color] = targets match {
    case head +: tail =>
      val newTargets = rules.filter(r => r.content._2 == head).map(_.container).filterNot(checked.contains).toSet
      rec1((newTargets ++ tail).toSeq, checked + head, rules)
    case Seq() => checked
  }

  override def part2(rules: Seq[Rule]): Int = countInner("shiny gold", rules) - 1

  def countInner(color: String, rules: Seq[Rule]): Int =
    rules.filter(_.container == color).map(r => r.content._1 * countInner(r.content._2, rules)).sum + 1
}
