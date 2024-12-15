package com.lmat.adventofcode.year2024

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

object Day14 extends SimpleCommonPuzzle[Seq[((Int, Int), (Int, Int))], BigInt, Int] {

  override def parse(resource: String): Seq[((Int, Int), (Int, Int))] =
    readResource(resource).map(
      line => ((
        line.split("=")(1).split(",")(0).toInt,
        line.split(" ")(0).split(",")(1).toInt
      ), (
        line.split("=")(2).split(",")(0).toInt,
        line.split(",")(2).toInt
      ))
    )

  val width = 101
  val height = 103
  val maxTime = 100

  def cap(position: (Int, Int)): (Int, Int) = (
    if (position._1 < 0) (width + (position._1 % width)) % width else position._1 % width,
    if (position._2 < 0) (height + (position._2 % height)) % height else position._2 % height
  )

  def afterTicks(initialPosition: (Int, Int), velocity: (Int, Int), count: Int): (Int, Int) =
    cap(
      initialPosition._1 + (velocity._1 * count),
      initialPosition._2 + (velocity._2 * count)
    )

  override def part1(input: Seq[((Int, Int), (Int, Int))]): BigInt = {


    def quadrant(position: (Int, Int)): Option[Int] = {
      val middleWidth = width / 2
      val middleHeight = height / 2
      if (position._1 < middleWidth && position._2 < middleHeight) Some(1)
      else if (position._1 > middleWidth && position._2 < middleHeight) Some(2)
      else if (position._1 < middleWidth && position._2 > middleHeight) Some(3)
      else if (position._1 > middleWidth && position._2 > middleHeight) Some(4)
      else None
    }

    input
      .map { case (pos, vel) => afterTicks(pos, vel, maxTime) }
      .groupBy(quadrant)
      .filter {
        case (Some(_), _) => true
        case (None, _) => false
      }
      .values
      .map(_.length)
      .map(BigInt(_))
      .product

  }

  override def part2(input: Seq[((Int, Int), (Int, Int))]): Int = {

    def arrToString(arr: Array[Array[Char]]): String = arr.map(l => l.mkString("", "", "")).mkString("", "\n", "\n\n")

    def seedToArr(seed: Int): (Int, Array[Array[Char]]) = {
      val test: Array[Array[Char]] = Array.ofDim[Char](width, height)
      input
        .map { case (pos, vel) => afterTicks(pos, vel, seed) }
        .tapEach {case(x,y) => test(x)(y) = 'X'}
      (seed, test)
    }

    val picture = LazyList.from(1)
      .map(seedToArr)
      .dropWhile { case (_, arr) => !arrToString(arr).contains("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")}.take(1).head

    println(arrToString(picture._2))
    picture._1
  }
}
