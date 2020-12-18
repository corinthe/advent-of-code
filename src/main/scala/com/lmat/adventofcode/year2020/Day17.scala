package com.lmat.adventofcode.year2020

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2020.Day17Definitions._
import com.lmat.util.Files.readResource

import scala.annotation.tailrec

object Day17Definitions {

  trait State

  case class Position(coords: Seq[Int]) {
    def addCoord(c: Int): Position = Position(coords :+ c)

    override def equals(obj: Any): Boolean = obj match {
      case o: Position => o.coords == coords
    }
  }

  case class Cube(position: Position, state: State) {
    def addCord(c: Int): Cube = Cube(position.addCoord(c), state)

    def neighboursPositions: Seq[Position] = {

      @tailrec
      def neighboursRec(depth: Int, pos: Seq[Position]): Seq[Position] = depth match {
        case d if d == position.coords.length => pos.filterNot(_ == position)
        case d if d == 0 => neighboursRec(d + 1, (position.coords(d) - 1 to position.coords(d) + 1).map(coord => Position(Seq(coord))))
        case d => neighboursRec(d + 1, (position.coords(d) - 1 to position.coords(d) + 1).flatMap(coord => pos.map(p => p.addCoord(coord))))
      }

      neighboursRec(0, Seq())
    }
  }

  case class PocketDimension(cubes: Seq[Cube]) {
    lazy val map: Map[Position, Cube] = cubes.map(c => c.position -> c).toMap
    lazy val neighboursCountActives: Map[Position, Int] =
      cubes
        .flatMap(c => c.neighboursPositions)
        .distinct
        .map(getCube)
        .map(c => (c, c.neighboursPositions.map(getCube)))
        .groupMapReduce(tuple => tuple._1.position)(tuple => tuple._2.count(_.state == Active))(_ + _)

    def countActive(): Int = cubes.count(_.state == Active)

    def changeDimensions(dims: Int): PocketDimension = {
      val correctDims = dims - cubes.head.position.coords.length
      if (correctDims <= 0) this
      else {
        @tailrec
        def changeRec(dimLeft: Int, currPocket: PocketDimension): PocketDimension = dimLeft match {
          case d if d < 0 => throw new IllegalArgumentException
          case 0 => currPocket
          case _ => changeRec(dimLeft - 1, PocketDimension(currPocket.cubes.map(c => c.addCord(0))))
        }

        changeRec(correctDims, this)
      }
    }

    def next(): PocketDimension = {
      PocketDimension(neighboursCountActives.map {
        case (pos, count) if getCube(pos).state == Active && (2 to 3).contains(count) => Cube(pos, Active)
        case (pos, count) if getCube(pos).state == Inactive && count == 3 => Cube(pos, Active)
        case (pos, _) => Cube(pos, Inactive)
        case _ => throw new IllegalStateException()
      }.toSeq)
    }

    def getCube(p: Position): Cube = {
      map.getOrElse(p, Cube(p, Inactive))
    }

  }

  case object Active extends State

  case object Inactive extends State

}

object Day17 extends SimpleCommonPuzzle[PocketDimension, Int, Int] {

  override def parse(resource: String): PocketDimension =
    PocketDimension(readResource(resource)
      .zipWithIndex
      .flatMap { case (line, x) => line
        .zipWithIndex
        .map { case (col, y) => Cube(Position(Seq(x, y)), if (col == '#') Active else Inactive) }
      })

  override def part1(input: PocketDimension): Int = {
    (1 to 6).foldLeft(input.changeDimensions(3)) { case (pocket, _) => pocket.next() }
      .countActive()
  }

  override def part2(input: PocketDimension): Int = {
    (1 to 6).foldLeft(input.changeDimensions(4)) { case (pocket, _) => pocket.next() }
      .countActive()
  }
}
