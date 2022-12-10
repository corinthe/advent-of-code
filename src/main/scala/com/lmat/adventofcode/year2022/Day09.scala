package com.lmat.adventofcode.year2022

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2022.Day09Definitions._
import com.lmat.util.Files.readResource

object Day09Definitions {

  case class Knot(x: Int = 0, y: Int = 0) {

    private def isCloseEnough(other: Knot): Boolean = (math.abs(x - other.x) <= 1) && (math.abs(y - other.y) <= 1)

    def moveTowards(other: Knot): Knot = {
      if (isCloseEnough(other)) this //no need to move
      else if (x == other.x && y < other.y) Knot(x, y + 1) //UP
      else if (x < other.x && y < other.y) Knot(x + 1, y + 1) //UP RIGHT
      else if (x < other.x && y == other.y) Knot(x + 1, y) //RIGHT
      else if (x < other.x && y > other.y) Knot(x + 1, y - 1) //DOWN RIGHT
      else if (x == other.x && y > other.y) Knot(x, y - 1) //DOWN
      else if (x > other.x && y > other.y) Knot(x - 1, y - 1) //DOWN LEFT
      else if (x > other.x && y == other.y) Knot(x - 1, y) //LEFT
      else Knot(x - 1, y + 1) //UP LEFT
    }
  }

  trait Motion

  case object Up extends Motion

  case object Down extends Motion

  case object Left extends Motion

  case object Right extends Motion

  case class Rope(knots: Seq[Knot]) {

    def move(m: Motion): Rope = {
      val head = knots.head
      val headP = m match {
        case Right => Knot(head.x + 1, head.y)
        case Left => Knot(head.x - 1, head.y)
        case Up => Knot(head.x, head.y + 1)
        case Down => Knot(head.x, head.y - 1)
        case _ => throw new IllegalStateException()
      }
      Rope(knots.tail.scanLeft(headP)((target, knot) => knot.moveTowards(target)))
    }
  }

  case object Rope {
    def ofLength(l: Int): Rope = Rope(Seq.fill(l)(Knot()))
  }

}

object Day09 extends SimpleCommonPuzzle[Seq[Motion], Int, Int] {

  def parseLine(s: String): Seq[Motion] = {
    val motion = s.head match {
      case 'R' => Right
      case 'L' => Left
      case 'U' => Up
      case 'D' => Down
    }
    Seq.fill(s.drop(2).toInt)(motion)
  }

  override def parse(resource: String): Seq[Motion] =
    readResource(resource).flatMap(parseLine)

  override def part1(input: Seq[Motion]): Int = input.scanLeft(Rope.ofLength(2))(_.move(_)).map(_.knots.last).distinct.size

  override def part2(input: Seq[Motion]): Int = input.scanLeft(Rope.ofLength(10))(_.move(_)).map(_.knots.last).distinct.size
}
