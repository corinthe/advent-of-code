package com.lmat.adventofcode.year2024

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2024.Day09Definitions.Disk
import com.lmat.util.Files.readResource

object Day09Definitions {

  trait Block

  case class File(id: Int) extends Block

  case object Empty extends Block

  case class Disk(diskMap: String) {
    private var blocks: Seq[Block] = diskMap
      .map(_.asDigit)
      .zipWithIndex
      .flatMap {
        case (size, pos) if pos % 2 == 0 => Seq.fill(size)(File(pos / 2))
        case (size, pos) if pos % 2 == 1 => Seq.fill(size)(Empty)
      }

    override def toString: String = blocks.map {
      case Empty => "."
      case File(id) => "" + id
      case _ => ""
    }.mkString

    def checksum(): BigInt =
      blocks.zipWithIndex.map {
        case (File(id), i) => id * i
        case (Empty, _) => 0
      }.map(BigInt(_)).sum

    def repair(): Unit = {

      def swap(seq: Seq[Block], i: Int, j: Int): Seq[Block] = {
        if (i < 0 || j < 0 || i >= seq.length || j >= seq.length) {
          throw new IllegalArgumentException("Invalid indices")
        }

        val array = seq.toArray // Convert to mutable Array
        val temp = array(i)
        array(i) = array(j)
        array(j) = temp
        array.toSeq
      }

      var left = 0
      var right = blocks.length - 1

      while (left < right) {
        (blocks(left), blocks(right)) match {
          case (Empty, File(_)) =>
            blocks = swap(blocks, left, right)
            left = left + 1
            right = right - 1
          case (File(_), Empty) =>
            right = right - 1
            left = left + 1
          case (File(_), File(_)) =>
            left = left + 1
          case (Empty, Empty) =>
            right = right - 1
        }
      }
    }
  }
}

object Day09 extends SimpleCommonPuzzle[Disk, BigInt, Int] {

  override def parse(resource: String): Disk =
    Disk(readResource(resource).head)

  override def part1(input: Disk): BigInt = {
    input.repair()
    input.checksum()
  }

  override def part2(input: Disk): Int = {
    println(input)
    0
  }
}
