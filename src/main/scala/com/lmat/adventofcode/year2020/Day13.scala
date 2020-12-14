package com.lmat.adventofcode.year2020

import com.lmat.adventofcode.SimpleMultiPuzzle
import com.lmat.adventofcode.year2020.Day13Definitions.{Id, Order, Timestamp}
import com.lmat.util.Files.readResource

object Day13Definitions {
  type Id = Int
  type Timestamp = Int
  type Order = Int
}

object Day13 extends SimpleMultiPuzzle[(Timestamp, Seq[Id]), Int, Seq[(Order, Id)], Long] {
  override def parse1(resource: String): (Timestamp, Seq[Id]) = {
    val r = readResource(resource)
    (r.head.toInt, parse1Line(r.last))
  }

  def parse1Line(input: String): Seq[Id] = input.split(",").flatMap(_.toIntOption).toSeq

  override def part1(input: (Timestamp, Seq[Id])): Int = {
    def bussesPassOn(time: Timestamp): Option[(Timestamp, Id)]=
      input._2.find(id => time % id == 0).map((time, _))

    val (timestamp, busId) = LazyList.from(input._1)
      .map(bussesPassOn)
      .find(_.isDefined)
      .get
      .get

    (timestamp - input._1) * busId
  }

  override def parse2(resource: String): Seq[(Id, Order)] = {
    parse2Line(readResource(resource).last)
  }


  def parse2Line(input: String): Seq[(Id, Order)] = input.split(",").map(_.toIntOption)
    .zipWithIndex
    .filter(_._1.isDefined)
    .map(a => (a._1.get, a._2))
    .toSeq

  override def part2(busses: Seq[(Id, Order)]): Long = {

    //it would take a while to compute otherwise
    val wolframInput = busses.map { case (a,b) => s"(x+$b) mod $a == 0"}.mkString(", ")
    println(wolframInput)

    //https://www.wolframalpha.com/input/?i=%28x%2B0%29+mod+37+%3D%3D+0%2C+%28x%2B27%29+mod+41+%3D%3D+0%2C+%28x%2B37%29+mod+601+%3D%3D+0%2C+%28x%2B49%29+mod+19+%3D%3D+0%2C+%28x%2B54%29+mod+17+%3D%3D+0%2C+%28x%2B60%29+mod+23+%3D%3D+0%2C+%28x%2B66%29+mod+29+%3D%3D+0%2C+%28x%2B68%29+mod+443+%3D%3D+0%2C+%28x%2B81%29+mod+13+%3D%3D+0

    0
  }
}
