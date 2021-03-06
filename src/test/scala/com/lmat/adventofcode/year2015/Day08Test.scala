package com.lmat.adventofcode.year2015

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks

class Day08Test extends AnyFunSuite with TableDrivenPropertyChecks {

  val raw =
    """""
      |"abc"
      |"aaa\"aaa"
      |"\x27"""".stripMargin

  val decoded =
    """
      |abc
      |aaa"aaa
      |'""".stripMargin

  val encoded =
    """"\"\""
      |"\"abc\""
      |"\"aaa\\\"aaa\""
      |"\"\\x27\""""".stripMargin

  test("Day08 - Resolve escapes") {
    assert(raw.split("\n").map(resolveEscapes) sameElements decoded.split("\n"))
  }

  test("Day08 - Add escapes") {
    assert(raw.split("\n").map(addEscapes) sameElements encoded.split("\n"))
  }
}
