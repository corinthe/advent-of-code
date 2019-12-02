package com.lmat.adventofcode.year2019

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.annotation.tailrec
import scala.util.Try

object Day02 extends SimpleCommonPuzzle[Array[Int], Int, Int] {

  val TARGET_OUTPUT: Int = 19690720;

  override def parse(resource: String): Array[Int] =
    readResource(resource).flatMap(row => row.split(",")).flatMap(int => Try(int.toInt).toOption).toArray

  override def part1(integers: Array[Int]): Int =
    runProgram(integers)(0)

  def runProgram(integers: Array[Int], noun: Int = 12, verb: Int = 2): Array[Int] = {

    @tailrec
    @throws(classOf[IllegalStateException])
    def runStep(array: Array[Int], position: Int): Array[Int] = array(position) match {
      case 1 => runStep(op(array, position, (x, y) => x + y), position + 4)
      case 2 => runStep(op(array, position, (x, y) => x * y), position + 4)
      case 99 => array
      case _ => throw new IllegalStateException("Erreur")
    }

    def op(array: Array[Int], position: Int, operation: (Int, Int) => Int): Array[Int] =
      array.updated(
        array(position + 3),
        operation(array(array(position + 1)), array(array(position + 2)))
      )

    runStep(integers.updated(1, noun).updated(2, verb), 0)
  }

  override def part2(integers: Array[Int]): Int =
    (for (x <- 0 to 99; y <- 0 to 99) yield (x, y))
      .flatMap({
        case (noun, verb) if runProgram(integers, noun, verb)(0) == TARGET_OUTPUT => Some((100 * noun) + verb)
        case (_, _) => None
      })(0)
}
