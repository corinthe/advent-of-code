package com.lmat.adventofcode.year2022

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2022.Day11Definitions.{Monkey, MonkeyOrder, Problem, WorryLevel}
import com.lmat.util.Files.readResource

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day11Definitions {

  type WorryLevel = BigInt
  type MonkeyOrder = Int

  case class Problem(monkeys: Seq[Monkey], inspections: Map[MonkeyOrder, BigInt], currentRound: Int = 0, targetRound: Int) {
    var newItems: Map[MonkeyOrder, Seq[WorryLevel]] = monkeys.groupMapReduce(_.order)(_.items)(_ ++ _)
    var test: Map[MonkeyOrder, BigInt] = Map.from(inspections)

    @tailrec
    final def compute: Problem = {
      if (currentRound == targetRound) this
      else {
        Problem(Seq.from(doAllMonkeys()), Map.from(test), currentRound + 1, targetRound).compute
      }
    }

    def doAllMonkeys(): Seq[Monkey] = {
      monkeys.foreach(doOne)

      monkeys.map(m => m.copy(items = newItems(m.order).map(worryLevel => worryLevel % (monkeys.map(_.divisibleBy).product))))
    }

    def doOne(m: Monkey): Unit = {
      newItems(m.order).map(m.inspect).foreach { case (level, order) => newItems += (order -> (newItems.getOrElse(order, Seq.empty) :+ level)) }
      newItems = newItems - m.order + (m.order -> Seq.empty)
      test += (m.order -> (test.getOrElse(m.order, BigInt(0)) + m.inspected))
    }

  }

  case class Monkey(order: MonkeyOrder, items: Seq[WorryLevel], opTypeAddition: Boolean, opValue: Int, divisibleBy: Int, monkeyIfTrue: MonkeyOrder, monkeyIfFalse: MonkeyOrder) {

    var inspected: BigInt = 0

    def inspect(item: WorryLevel): (WorryLevel, MonkeyOrder) = {
      inspected += 1
      val opRealValue: BigInt = if (opValue == -1) item else opValue
      val newWorryLevel = (if (opTypeAddition) item + opRealValue else item * opRealValue)
      (newWorryLevel, if (newWorryLevel % divisibleBy == 0) monkeyIfTrue else monkeyIfFalse)
    }

    override def toString: String =
      s"""Monkey $order:
         |  Starting items: ${items.mkString(", ")}
         |  Operation: new = old ${if (opTypeAddition) "+" else "*"} ${if (opValue == -1) "old" else opValue}
         |  Test: divisible by $divisibleBy
         |    If true: throw to monkey $monkeyIfTrue
         |    If false: throw to monkey $monkeyIfFalse""".stripMargin
  }
}

object Day11 extends SimpleCommonPuzzle[Seq[Monkey], BigInt, BigInt] {

  def parseBlock(block: Seq[String]): Monkey = {
    val order: MonkeyOrder = block.head.filter(_.isDigit).toInt

    val itemPattern: Regex = """(?:(\d+)(?:, )?)""".r
    val items: Seq[WorryLevel] = {
      itemPattern.findAllMatchIn(block(1).drop("  Starting items: ".length)).map(_.group(1)).map(_.toInt).map(BigInt(_)).toSeq

    }
    val opTypeAddition: Boolean = block(2).drop("  Operation: new = old ".length).head == '+'
    val opValue: Int = if (block(2).drop("  Operation: new = old ".length + 2).equals("old")) -1 else block(2).drop("  Operation: new = old ".length + 2).toInt
    val divisibleBy: Int = block(3).drop("  Test: divisible by ".length).toInt
    val monkeyIfTrue: MonkeyOrder = block(4).drop("    If true: throw to monkey ".length).toInt
    val monkeyIfFalse: MonkeyOrder = block(5).drop("    If false: throw to monkey ".length).toInt
    Monkey(order, items, opTypeAddition, opValue, divisibleBy, monkeyIfTrue, monkeyIfFalse)
  }

  override def parse(resource: String): Seq[Monkey] =
    readResource(resource).grouped(7).map(parseBlock).toSeq

  override def part1(input: Seq[Monkey]): BigInt = {

    Problem(input, Map[MonkeyOrder, BigInt](), 0, 20)
      .compute
      .inspections
      .values.toSeq
      .sorted
      .reverse
      .take(2)
      .product

  }

  override def part2(input: Seq[Monkey]): BigInt = {

    Problem(input, Map[MonkeyOrder, BigInt](), 0, 10000)
      .compute
      .inspections
      .values.toSeq
      .sorted
      .reverse
      .take(2)
      .product

  }
}
