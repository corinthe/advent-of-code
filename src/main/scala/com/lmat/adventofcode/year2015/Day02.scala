package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2015.Day02Definitions.Gift
import com.lmat.util.Files.readResource

import scala.util.matching.Regex

object Day02Definitions {
  case class Gift(l: BigInt, w: BigInt, h: BigInt) {
    def wrapping: BigInt = {
      (2 * l * w) + (2 * w * h) + (2 * h * l) + Seq(l, w, h).combinations(2).map(_.product).min
    }

    def ribbon: BigInt = {
      (Seq(l, w, h).sorted.take(2).sum  * 2) + Seq(l,w,h).product
    }
  }
}

object Day02 extends SimpleCommonPuzzle[Seq[Gift], BigInt, BigInt] {

  val PATTERN: Regex = """(.+)x(.+)x(.+)""".r

  override def parse(resource: String): Seq[Gift] =
    readResource(resource).map {
      case PATTERN(l, w, h) => Gift(l.toInt, w.toInt, h.toInt)
    }

  override def part1(input: Seq[Gift]): BigInt = input.map(_.wrapping).sum

  override def part2(input: Seq[Gift]): BigInt = input.map(_.ribbon).sum
}
