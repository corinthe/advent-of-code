package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.{CommonPuzzle, SimpleCommonPuzzle}
import com.lmat.adventofcode.year2023.Day02Definitions._
import com.lmat.util.Files.readResource

import scala.util.matching.Regex

object Day02Definitions {
  trait Color

  case object Red extends Color

  case object Blue extends Color

  case object Green extends Color

  case class Draw(color: Color, number: Int)

  case class Rule(maxRed: Int, maxBlue: Int, maxGreen: Int)

  case class Game(id: Int, allDraws: Seq[Seq[Draw]]) {
    private def min(c: Color) = allDraws.map(draws => draws.find(draw => draw.color == c).map(_.number).getOrElse(0)).max

    def followsRule(r: Rule): Boolean = {
      allDraws.forall(draws => draws.forall(draw => draw.color match {
        case Red => r.maxRed >= draw.number
        case Green => r.maxGreen >= draw.number
        case Blue => r.maxBlue >= draw.number
      }))
    }

    def power: Int = min(Red) * min(Green) * min(Blue)
  }
}

object Day02 extends CommonPuzzle[Seq[String], Seq[Game], Int, Int] {

  override def parse(resource: String): Seq[String] =
    readResource(resource)

  override def preProcess(raw: Seq[String]): Seq[Game] =
    raw.map(parseGame)

  private def parseGame(s: String): Game = {

    def parseAllDraws(allDraws: String): Seq[Seq[Draw]] = {
      def parseDraws(draws: String): Seq[Draw] = {
        def parseDraw(draw: String): Draw = {
          val RED_DRAW: Regex = """(.*)red""".r
          val BLUE_DRAW: Regex = """(.*)blue""".r
          val GREEN_DRAW: Regex = """(.*)green""".r

          draw match {
            case RED_DRAW(number) => Draw(Red, number.toInt)
            case BLUE_DRAW(number) => Draw(Blue, number.toInt)
            case GREEN_DRAW(number) => Draw(Green, number.toInt)
          }
        }

        draws.replaceAll(" ", "").split(",").map(parseDraw)
      }

      val r = allDraws.split(";").map(parseDraws)
      r
    }

    val GAME_PATTERN: Regex = """Game (.*): (.*)""".r
    s match {
      case GAME_PATTERN(gameId, rest) => Game(gameId.toInt, parseAllDraws(rest))
      case _ => throw new IllegalStateException("Erreur de parsing")
    }
  }

  override def part1(games: Seq[Game]): Int =
    games.filter(_.followsRule(Rule(12, 14, 13))).map(_.id).sum

  override def part2(games: Seq[Game]): Int =
    games.map(_.power).sum
}
