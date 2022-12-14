package com.lmat.adventofcode.year2022

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2022.Day12Definitions.{Grid, Heighmap, Point}
import com.lmat.util.Files.readResource


object Day12Definitions {

  case class Point(x: Int, y: Int) extends Equals {
    def delta(dx: Int, dy: Int): Point = Point(x + dx, y + dy)
  }

  case class Heighmap(x: Map[Point, Char]) extends Grid[Char](x) {
    override def value(point: Point): Int = x(point) match {
      case 'S' => 'a'
      case 'E' => 'z'
      case any => any
    }
  }

  abstract class Grid[A](map: Map[Point, A]) {
    def neighbours(point: Point): Seq[Point] = Seq((1, 0), (-1, 0), (0, 1), (0, -1)).map(p => point.delta(p._1, p._2)).filter(map.contains)

    def value(point: Point): Int

    def bfs(start: A => Boolean, end: A): Seq[Seq[Point]] = {
      val swapped = map.filter { case (_, v) => start(v) }.keys.toSeq
      swapped.flatMap(p =>bfs(p, map.map(_.swap)(end)))
    }


    def bfs(startP: Point, endP: Point): Option[Seq[Point]] = {
      val todo = collection.mutable.Queue(startP)
      val path = collection.mutable.Map(startP -> Seq(startP))

      do {
        val curr = todo.dequeue()
        if (curr == endP) {
          return Some(path(curr).init)
        }
        neighbours(curr).filterNot(path.contains).foreach { neighbour =>
          if (value(neighbour) - value(curr) <= 1) {
            todo.enqueue(neighbour)
            path(neighbour) = path(curr) :+ neighbour
          }

        }
      } while (todo.nonEmpty)
      None
    }
  }

}

object Day12 extends SimpleCommonPuzzle[Heighmap, Int, Int] {

  override def parse(resource: String): Heighmap = {
    val lines = readResource(resource)
    val points = for {
      y <- lines.indices
      x <- lines(y).indices
    } yield Point(x, y) -> lines(y)(x)
    Heighmap(points.toMap)
  }

  override def part1(input: Heighmap): Int = {
    input.bfs(x => x == 'S', 'E').map(_.length).min
  }

  override def part2(input: Heighmap): Int = {
    input.bfs(x => x == 'S' || x == 'a', 'E').map(_.length).min
  }

}
