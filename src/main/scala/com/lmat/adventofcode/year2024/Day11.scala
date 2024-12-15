package com.lmat.adventofcode.year2024

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

object Day11 extends SimpleCommonPuzzle[LazyList[BigInt], Int, Int] {

  override def parse(resource: String): LazyList[BigInt] =
    LazyList.from(readResource(resource).head.split(" ").map(BigInt(_)))

  def split(input: BigInt): LazyList[BigInt] = {
    val (firstHalf, secondHalf) = input.toString().splitAt(input.toString().length / 2)
    LazyList(BigInt(firstHalf), BigInt(secondHalf))
  }

  override def part1(input: LazyList[BigInt]): Int = {

    val zero = BigInt(0)
    LazyList.iterate(input)(s => s.flatMap {
      case `zero` => LazyList(1)
      case n if n.toString().length % 2 == 0 => split(n)
      case n => LazyList(n * 2024)
    }).take(26).last.length
  }

  override def part2(input: LazyList[BigInt]): Int = {
    val zero = BigInt(0)
    LazyList.iterate(input)(s => s.flatMap {
      case `zero` => LazyList(1)
      case n if n.toString().length % 2 == 0 => split(n)
      case n => LazyList(n * 2024)
    }).take(26).last.length
  }
}
