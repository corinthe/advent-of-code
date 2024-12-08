package com.lmat.adventofcode.year2024

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

object Day07 extends SimpleCommonPuzzle[Seq[(BigInt, Seq[BigInt])], BigInt, BigInt] {

  override def parse(resource: String): Seq[(BigInt, Seq[BigInt])] = readResource(resource).map {
    line =>
      (
        BigInt(line.split(":").head),
        line.split(": ").last.split(" ").map(BigInt(_))
      )
  }

  override def part1(input: Seq[(BigInt, Seq[BigInt])]): BigInt = input
    .filter(elem =>
      elem
        ._2
        .tail
        .foldLeft(Seq(elem._2.head))((s: Seq[BigInt], v: BigInt) => s.flatMap(e => Seq(e + v, e * v)))
        .contains(elem._1))
    .map(_._1)
    .sum

  override def part2(input: Seq[(BigInt, Seq[BigInt])]): BigInt = input
    .filter(elem =>
      elem
        ._2
        .tail
        .foldLeft(Seq(elem._2.head))((s: Seq[BigInt], v: BigInt) => s.flatMap(e => Seq(e + v, e * v, BigInt(s"$e$v"))))
        .contains(elem._1))
    .map(_._1)
    .sum
}
