package com.lmat.adventofcode.year2021

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2021.Day22Definitions.Cuboid
import com.lmat.util.Files.readResource

import scala.collection.mutable
import scala.util.matching.Regex

object Day22Definitions {
  type Cube = (Boolean, Int, Int, Int)
  type Pos = (Int, Int, Int)
  case class Cuboid(on: Boolean, xs: (Int, Int), ys: (Int, Int), zs: (Int, Int)) {
    def toCubes: Seq[Cube] = {
      for {
        x <- xs._1 to xs._2 if x >= -50 && x <= 50
        y <- ys._1 to ys._2 if y >= -50 && y <= 50
        z <- zs._1 to zs._2 if z >= -50 && z <= 50
      } yield (on,x,y,z)
    }
  }
}

object Day22 extends SimpleCommonPuzzle[Seq[Cuboid], Int, Int] {

  override def parse(resource: String): Seq[Cuboid] =
    readResource(resource).map(parseLine)

  def parseLine(line: String): Cuboid = {
    val PATTERN: Regex = """(.*) x=(.*)\.\.(.*),y=(.*)\.\.(.*),z=(.*)\.\.(.*)""".r
    line match {
      case PATTERN(on, x1, x2, y1, y2, z1, z2) => Cuboid(on == "on",(x1.toInt, x2.toInt), (y1.toInt, y2.toInt), (z1.toInt, z2.toInt))
      case _ => throw new IllegalStateException("parsing impossible")
    }
  }

  override def part1(input: Seq[Cuboid]): Int = {

    val set = mutable.Set[(Int, Int, Int)]()
    input
      .flatMap(_.toCubes)
      .filter(cube => cube._2 >= -50 && cube._2 <= 50)
      .filter(cube => cube._3 >= -50 && cube._3 <= 50)
      .filter(cube => cube._4 >= -50 && cube._4 <= 50)
      .foldLeft(set)((acc, curr) => if(curr._1) acc += ((curr._2, curr._3, curr._4)) else acc -= ((curr._2, curr._3, curr._4)))
      .size
  }

  override def part2(input: Seq[Cuboid]): Int = 0
}
