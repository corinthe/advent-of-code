package com.lmat.adventofcode.year2022

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2022.Day04Definitions.SectionAssignment
import com.lmat.util.Files.readResource

import scala.util.matching.Regex

object Day04Definitions {
  type Section = Int

  case class SectionAssignment(begin: Section, end: Section) {
    def sections(): Seq[Section] = begin to end

    def fullyContains(other: SectionAssignment): Boolean = begin <= other.begin && end >= other.end

    def overlaps(other: SectionAssignment): Boolean = sections().intersect(other.sections()).nonEmpty
  }
}

object Day04 extends SimpleCommonPuzzle[Seq[(SectionAssignment, SectionAssignment)], Int, Int] {

  val ASSIGNMENT_PATTERN: Regex = """(.*)-(.*),(.*)-(.*)""".r

  override def parse(resource: String): Seq[(SectionAssignment, SectionAssignment)] = readResource(resource).map {
    case ASSIGNMENT_PATTERN(b1, e1, b2, e2) => (SectionAssignment(b1.toInt, e1.toInt), SectionAssignment(b2.toInt, e2.toInt))
    case _ => throw new IllegalStateException("Illegal instruction")
  }

  override def part1(input: Seq[(SectionAssignment, SectionAssignment)]): Int =
    input.count(pair => pair._1.fullyContains(pair._2) || pair._2.fullyContains(pair._1))

  override def part2(input: Seq[(SectionAssignment, SectionAssignment)]): Int =
    input.count(pair => pair._1.overlaps(pair._2) || pair._2.overlaps(pair._1))
}
