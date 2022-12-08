package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2015.Day03Definitions.{Direction, Down, Up, Left, Right}
import com.lmat.util.Files.readResource

object Day03Definitions {

  type Location = (Int, Int)

  trait Direction {
    def exec(l: Location): Location
  }

  case object Up extends Direction {
    override def exec(l: (Int, Int)): (Int, Int) = (l._1 + 1, l._2)
  }
  case object Down extends Direction {
    override def exec(l: (Int, Int)): (Int, Int) = (l._1 - 1, l._2)
  }
  case object Left extends Direction {
    override def exec(l: (Int, Int)): (Int, Int) = (l._1, l._2 - 1)
  }
  case object Right extends Direction {
    override def exec(l: (Int, Int)): (Int, Int) = (l._1, l._2 + 1)
  }
}

object Day03 extends SimpleCommonPuzzle[Seq[Direction], Int, Int] {

  override def parse(resource: String): Seq[Direction] =
    readResource(resource).head.map {
      case '^' => Up
      case 'v' => Down
      case '<' => Left
      case '>' => Right
    }

  override def part1(input: Seq[Direction]): Int =
    input.scanLeft((0, 0)) { case (l, d) => d.exec(l)}.distinct.size

  override def part2(input: Seq[Direction]): Int =  {
    val santaInputs = input.zipWithIndex.filter { case (_, i) => i % 2 == 1}.map { case (d, _) => d}
    val roboSantaInput = input.zipWithIndex.filter { case (_, i) => i % 2 == 0}.map { case (d, _) => d}

    (santaInputs.scanLeft((0, 0)) { case (l, d) => d.exec(l)} ++ roboSantaInput.scanLeft((0, 0)) { case (l, d) => d.exec(l)}).distinct.size
  }


}
