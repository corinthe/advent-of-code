package com.lmat.adventofcode.year2020

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2020.Day16Definitions.{FieldDefinition, Problem, Ticket}
import com.lmat.util.Files.readResource

import scala.util.matching.Regex

object Day16Definitions {

  type Field = String

  case class FieldDefinition(field: Field, ranges: Seq[Range]) {
    def includes(value: BigInt): Boolean =
      ranges.exists(r => r.contains(value))

    override def toString: Field =
      s"$field: ${ranges.head.start}-${ranges.head.`end`} or ${ranges.last.start}-${ranges.last.`end`}"
  }

  case class Ticket(values: Seq[BigInt]) {
    def invalidValues(rules: Seq[FieldDefinition]): Seq[BigInt] = {
      val allRanges = rules.flatMap(_.ranges)
      values.filterNot(v => allRanges.exists(_.contains(v)))
    }

    def respectsFields(rules: Seq[FieldDefinition]): Boolean =
      rules.zip(values).forall { case (r, v) => r.includes(v) }

    override def toString: Field = values.mkString(",")
  }

  case class Problem(fieldDefinitions: Seq[FieldDefinition], yourTicket: Ticket, nearbyTickets: Seq[Ticket]) {

    private lazy val validTickets: Seq[Ticket] = nearbyTickets.filter(_.invalidValues(fieldDefinitions).isEmpty)

    override def toString: Field = {
      fieldDefinitions.mkString("\n") + "\n\nyour ticket:\n" + yourTicket.toString + "\n\nnearby tickets: \n" + nearbyTickets.mkString("\n")
    }

    def solve1(): BigInt = nearbyTickets.flatMap(_.invalidValues(fieldDefinitions)).sum

    def solve2(): BigInt =
      fieldDefinitions
        //for each field, find all valid positions
        .map(f => (f.field, fieldDefinitions.indices.filter(i => validTickets.forall(t => f.includes(t.values(i))))))
        //sort to have the field with the less possibilities first
        .sorted(Ordering.by[(String, Seq[Int]), Int](_._2.length))
        //reverse to have the field with the less possibilities last (in order to put in map after)
        .reverse
        .flatMap { case (field, positions) => positions.map((_, field)) }
        //insert all possibilities in a map
        //since we sorted and reversed our possibilities, the last ones will override the first ones
        .toMap
        //only keep the departures fields
        .filter { case (_, field) => field.contains("departure") }
        .map { case (index, _) => yourTicket.values(index) }
        .product

  }

}

object Day16 extends SimpleCommonPuzzle[Problem, BigInt, BigInt] {

  override def parse(resource: String): Problem =
    parseProblem(readResource(resource))

  def parseProblem(input: Seq[String]): Problem = {
    val FIELD_PATTERN: Regex = """(.+): (\d+)-(\d+) or (\d+)-(\d+)""".r

    val fields: Seq[FieldDefinition] = input.filter(l => FIELD_PATTERN.matches(l)).map {
      case FIELD_PATTERN(name, r1min, r1max, r2min, r2max) => FieldDefinition(name, Seq(r1min.toInt to r1max.toInt, r2min.toInt to r2max.toInt))
    }

    val tickets = input.filter(l => l.contains(","))
    val myTicket = Ticket(tickets.head.split(",").map(BigInt(_)).toSeq)
    val nearbyTickets = tickets.tail.map(l => Ticket(l.split(",").map(BigInt(_)).toSeq))

    Problem(fields, myTicket, nearbyTickets)
  }

  override def part1(input: Problem): BigInt = input.solve1()

  override def part2(input: Problem): BigInt = input.solve2()
}
