package com.lmat.adventofcode.year2022

import com.lmat.adventofcode.{SimpleCommonPuzzle, SimpleMultiPuzzle}
import com.lmat.adventofcode.year2022.Day02Definitions.{Draw, Lose, Outcome, Paper, Rock, Round, Scissors, Shape, StrategyGuide, Win}
import com.lmat.util.Files.readResource

import scala.util.matching.Regex

object Day02Definitions {
  trait Shape {
    def value(): Int
  }

  case object Rock extends Shape {
    override def value(): Int = 1
  }
  case object Paper extends Shape {
    override def value(): Int = 2
  }
  case object Scissors extends Shape {
    override def value(): Int = 3
  }

  trait Outcome {
    def value(): Int
  }

  case object Win extends Outcome {
    override def value(): Int = 6
  }
  case object Draw extends Outcome {
    override def value(): Int = 3
  }
  case object Lose extends Outcome {
    override def value(): Int = 0
  }

  case class Round(opMove: Shape, myMove: Shape) {
    def score(): Int = (opMove, myMove) match {
      case (Rock, Rock) => Draw.value() + myMove.value()
      case (Rock, Paper) => Win.value() + myMove.value()
      case (Rock, Scissors) => Lose.value() + myMove.value()
      case (Paper, Rock) => Lose.value() + myMove.value()
      case (Paper, Paper) => Draw.value() + myMove.value()
      case (Paper, Scissors) => Win.value() + myMove.value()
      case (Scissors, Rock) => Win.value() + myMove.value()
      case (Scissors, Paper) => Lose.value() + myMove.value()
      case (Scissors, Scissors) => Draw.value() + myMove.value()
      case _ => throw new IllegalStateException("Illegal instruction")
    }
  }

  case class StrategyGuide(rounds: Seq[Round]) {
    def score(): Int = rounds.map(_.score()).sum
  }
}

object Day02 extends SimpleMultiPuzzle[StrategyGuide, Int, Seq[(Shape, Outcome)], Int] {

  val ROUND_PATTERN: Regex = """(.) (.)""".r

  override def parse1(resource: String): StrategyGuide =
    StrategyGuide(readResource(resource).map(parseRow1))

  def parseRow1(row: String): Round = row match {
    case ROUND_PATTERN(op, me) => Round(parseShape(op.head), parseShape(me.head))
  }

  def parseShape(c: Char): Shape = c match {
    case 'A' => Rock
    case 'X' => Rock
    case 'B' => Paper
    case 'Y' => Paper
    case 'C' => Scissors
    case 'Z' => Scissors
    case _ => throw new IllegalStateException("Illegal instruction")
  }

  override def part1(input: StrategyGuide): Int = input.score()



  override def part2(input: Seq[(Shape, Outcome)]): Int =
    input.map({ case (s, o) => (s, o) match {
      case (Rock, Win) => Paper.value() + o.value()
      case (Paper, Win) => Scissors.value() + o.value()
      case (Scissors, Win) => Rock.value() + o.value()
      case (Rock, Draw) => Rock.value() + o.value()
      case (Paper, Draw) => Paper.value() + o.value()
      case (Scissors, Draw) => Scissors.value() + o.value()
      case (Rock, Lose) => Scissors.value() + o.value()
      case (Paper, Lose) => Rock.value() + o.value()
      case (Scissors, Lose) => Paper.value() + o.value()
      case _ => throw new IllegalStateException("Illegal instruction")
    }
    }).sum

  override def parse2(resource: String): Seq[(Shape, Outcome)] =
    readResource(resource).map(parseRow2)

  def parseRow2(row: String): (Shape, Outcome) = row match {
    case ROUND_PATTERN(op, me) => (parseShape(op.head), parseOutcome(me.head))
  }

  def parseOutcome(c: Char): Outcome = c match {
    case 'X' => Lose
    case 'Y' => Draw
    case 'Z' => Win
    case _ => throw new IllegalStateException("Illegal instruction")
  }

}
