package com.lmat.adventofcode.year2020

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2020.Day14Definitions._
import com.lmat.util.Files.readResource

import scala.collection.mutable
import scala.util.matching.Regex

object Day14Definitions {
  type Address = Long
  type Value = Long

  trait Instruction

  case class SetMask(value: String) extends Instruction

  case class SetMemory(address: Address, value: Value) extends Instruction

  case class Problem1(private val instructions: Seq[Instruction]) {
    private val memory: mutable.Map[Address, Value] = mutable.Map()

    private var currMaskEffect: Long => Long = _ => throw new IllegalStateException()

    def solve(): Long = {
      instructions.foreach {
        case SetMask(value) => currMaskEffect = setMask(value)
        case SetMemory(address, value) => memory(address) = currMaskEffect(value)
        case _ => throw new IllegalStateException()
      }

      memory.values.sum
    }

    private def setMask(mask: String): Long => Long = {
      (initialValue: Long) => {
        val binaryString = initialValue.toBinaryString.reverse.zipAll(mask.reverse, '0', '0').map {
          case (input, mask) => mask match {
            case 'X' => input
            case _ => mask
          }
        }.mkString.reverse
        BigInt(binaryString, 2).longValue
      }

    }
  }

  implicit class StringImprovements(s: String) {
    def multiReplaceAll(target: Char, replacements: String): Seq[String] = {
      var result: Seq[String] = Seq(s)
      while (result.exists(s => s.contains(target))) {
        result = result.flatMap(_.multiReplace(target, replacements))
      }
      result
    }

    def multiReplace(target: Char, replacements: String): Seq[String] =
      if (s.contains(target)) replacements.map(c => s.replaceFirst("" + target, "" + c))
      else Seq(s)
  }

  case class Problem2(private val instructions: Seq[Instruction]) {
    private val memory: mutable.Map[Address, Value] = mutable.Map()

    private var currMaskEffect: Long => Seq[Long] = _ => throw new IllegalStateException()

    def solve(): Long = {
      instructions.foreach {
        case SetMask(value) => currMaskEffect = setMask(value)
        case SetMemory(address, value) => currMaskEffect(address).foreach(ad => memory(ad) = value)
        case _ => throw new IllegalStateException()
      }

      memory.values.sum
    }

    private def setMask(mask: String): Long => Seq[Long] = {
      def maskedAddressToInstructions(maskedAddress: String): Seq[Address] =
        maskedAddress
          .multiReplaceAll('X', "01")
          .map(BigInt(_, 2).longValue)

      (address: Long) =>
        maskedAddressToInstructions(
          address.toBinaryString.reverse.zipAll(mask.reverse, '0', '0').map {
            case (input, mask) => mask match {
              case '0' => input
              case _ => mask
            }
          }.mkString.reverse)
    }
  }

}

object Day14 extends SimpleCommonPuzzle[Seq[Instruction], Long, Long] {
  val MASK_PATTERN: Regex = """mask = (.*)""".r
  val MEMORY_PATTERN: Regex = """mem\[(.*)] = (.*)""".r

  override def parse(resource: String): Seq[Instruction] =
    readResource(resource).map(parseLine)

  def parseLine(input: String): Instruction = input match {
    case MASK_PATTERN(maskValue) => SetMask(maskValue)
    case MEMORY_PATTERN(address, value) => SetMemory(address.toInt, value.toInt)
    case _ => throw new IllegalStateException()
  }

  override def part1(input: Seq[Instruction]): Long =
    Problem1(input).solve()

  override def part2(input: Seq[Instruction]): Long =
    Problem2(input).solve()
}
