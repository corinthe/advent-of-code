package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.annotation.tailrec

object Day04 extends SimpleCommonPuzzle[Seq[String], Int, Int] {

  override def parse(resource: String): Seq[String] =
    readResource(resource)

  override def part1(input: Seq[String]): Int =
    input
      .map(i => i.split(":").last)
      .map(i => i.split("\\|"))
      .map(ticket => (ticket.head.strip().replace("  ", " ").split(" ").map(_.toInt).toSeq, ticket.last.strip().replace("  ", " ").split(" ").map(_.toInt).toSeq))
      .map(ticket => ticket._2.count(actual => ticket._1.contains(actual)))
      .map(matches => if (matches == 0) 0 else Math.pow(2, matches - 1).toInt)
      .sum

  override def part2(input: Seq[String]): Int = {

    val initialTickets = input
      .map(i => i.split(":").last)
      .map(i => i.split("\\|"))
      .map(ticket => (ticket.head.strip().replace("  ", " ").split(" ").map(_.toInt).toSeq, ticket.last.strip().replace("  ", " ").split(" ").map(_.toInt).toSeq))
      .zipWithIndex
      .map(t => (t._2 + 1, t._1._1, t._1._2))

    @tailrec
    def checkTickets(tickets: Seq[(Int, Seq[Int], Seq[Int])], ticketsWon: Seq[Int] = Seq()): Seq[Int] = {
      if (tickets.isEmpty) return ticketsWon

      val winners = tickets
        .filter(ticket => ticket._3.exists(actual => ticket._2.contains(actual)))


      val won = winners.flatMap(ticket => initialTickets.slice(ticket._1, ticket._1 + ticket._3.count(actual => ticket._2.contains(actual)))).map(_._1)


      checkTickets(winners
        .flatMap(ticket => initialTickets.slice(ticket._1, ticket._1 + ticket._3.count(actual => ticket._2.contains(actual))))
        , ticketsWon ++ won)
    }

    checkTickets(
      initialTickets,
      initialTickets.map(_._1)
    ).length
  }

}
