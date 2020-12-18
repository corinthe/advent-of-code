package com.lmat.adventofcode.year2020

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.util.matching.Regex

object Day18 extends SimpleCommonPuzzle[Seq[String], BigInt, BigInt] {
  private val add: Regex = "(.+)\\+(.+)".r
  private val mul: Regex = "(.+)\\*(.+)".r
  private val par: Regex = raw"(.*)\(([^\(\)]+)\)(.*)".r
  private val addOrMul: Regex = "(.+)([+*])(.+)".r

  def eval1(expr: String): BigInt = {
    expr match {
      case par(left, expr, right) => eval1(s"$left${eval1(expr)}$right")
      case addOrMul(x, op, y) => op match {
        case "+" => eval1(x) + eval1(y)
        case "*" => eval1(x) * eval1(y)
      }
      case n => BigInt(n)
    }
  }

  def eval2(expr: String): BigInt =
    expr match {
      case par(left, expr, right) => eval2(s"$left${eval2(expr)}$right")
      case mul(x, y) => eval2(x) * eval2(y)
      case add(x, y) => eval2(x) + eval2(y)
      case n => BigInt(n)
    }

  override def parse(resource: String): Seq[String] =
    readResource(resource).map(_.filterNot(c => c == ' '))

  override def part1(input: Seq[String]): BigInt = {
    input.map(eval1).sum
  }

  override def part2(input: Seq[String]): BigInt =
    input.map(eval2).sum
}
