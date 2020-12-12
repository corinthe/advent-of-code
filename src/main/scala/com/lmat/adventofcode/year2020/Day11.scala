package com.lmat.adventofcode.year2020

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2020.Day11Definitions._
import com.lmat.util.Files.readResource

import scala.annotation.tailrec
import scala.language.implicitConversions


object Day11Definitions {

  implicit def tupleToPosition(t: (Int, Int)): Position = Position(t._1, t._2)

  trait Status

  trait Direction

  case class Position(i: Int, j: Int) {
    def go(dir: Direction): Position = dir match {
      case Up => Position(i - 1, j)
      case Down => Position(i + 1, j)
      case Left => Position(i, j - 1)
      case Right => Position(i, j + 1)
      case UpLeft => Position(i - 1, j - 1)
      case UpRight => Position(i - 1, j + 1)
      case DownLeft => Position(i + 1, j - 1)
      case DownRight => Position(i + 1, j + 1)
    }

  }

  case class Layout(positions: Array[Array[Status]]) {
    def next(part: Int): Layout =
      if(part == 1) next(directNeighbours, 4)
      else next(visibleNeighbours, 5)

    def next(f: Position => Seq[Status], occupiedLimit: Int): Layout =
      Layout(positions.zipWithIndex.map { case (lines, i) => lines.zipWithIndex.map { case (status, j) =>
        if (status == Empty && !f((i, j)).contains(Occupied)) Occupied
        else if (status == Occupied && f((i, j)).count(_ == Occupied) >= occupiedLimit) Empty
        else status
      }
      })

    def directNeighbours(from: Position): Seq[Status] = validNeighboursPositions(from).map(getStatus).filter(_ != Floor)

    def validNeighboursPositions(from: Position): Seq[Position] = {
      val is = (from.i - 1 to from.i + 1).filter(positions.indices.contains)
      val js = (from.j - 1 to from.j + 1).filter(positions(0).indices.contains)
      val res = for (i <- is; j <- js) yield Position(i, j)
      res.filterNot(_ == from)
    }

    def next2(): Layout =
      Layout(positions.zipWithIndex.map { case (lines, i) => lines.zipWithIndex.map { case (status, j) =>
        if (status == Empty && !visibleNeighbours((i, j)).contains(Occupied)) Occupied
        else if (status == Occupied && visibleNeighbours((i, j)).count(_ == Occupied) >= 5) Empty
        else status
      }
      })

    def visibleNeighbours(from: Position): Seq[Status] = {
      @tailrec
      def visibleNeighboursRec(currentPos: Position, direction: Direction): Option[Status] =
        if (currentPos.i < 0 || currentPos.i >= positions.length || currentPos.j < 0 || currentPos.j >= positions(0).length) None
        else if (currentPos != from && getStatus(currentPos) != Floor) Some(getStatus(currentPos))
        else visibleNeighboursRec(currentPos.go(direction), direction)

      Seq(Up, Down, Left, Right, UpLeft, UpRight, DownLeft, DownRight)
        .flatMap(d => visibleNeighboursRec(from, d))
    }

    def getStatus(pos: Position): Status = positions(pos.i)(pos.j)

    override def equals(obj: Any): Boolean = obj match {
      case o: Layout => o.positions.flatten.toSeq == positions.flatten.toSeq
    }

    override def toString: String = positions.map(line => line.mkString("")).mkString("\n")
  }

  case object Up extends Direction

  case object Down extends Direction

  case object Left extends Direction

  case object Right extends Direction

  case object UpLeft extends Direction

  case object UpRight extends Direction

  case object DownLeft extends Direction

  case object DownRight extends Direction

  case object Empty extends Status {
    override def toString: String = "L"
  }

  case object Floor extends Status {
    override def toString: String = "."
  }

  case object Occupied extends Status {
    override def toString: String = "#"
  }


}

object Day11 extends SimpleCommonPuzzle[Layout, Int, Int] {
  override def parse(resource: String): Layout = parseInput(readResource(resource))

  def parseInput(input: Seq[String]): Layout = {
    Layout(input
      .toArray
      .map(i => i.toCharArray.map {
        case 'L' => Empty
        case '.' => Floor
        case _ => throw new IllegalStateException("Illegal instruction")
      }))
  }

  private def computeResult(input: Layout, part: Int): Int =
    LazyList
    .iterate(input)(_.next(part))
    .sliding(2)
    .takeWhile(c => c.head != c.last)
    .map(_.last)
    .toSeq
    .last
    .positions
    .flatten
    .count(_ == Occupied)

  override def part1(input: Layout): Int = computeResult(input, 1)

  override def part2(input: Layout): Int = computeResult(input, 2)


}
