package com.lmat.adventofcode.year2023

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

import scala.annotation.tailrec

object Day01 extends SimpleCommonPuzzle[Seq[String], Int, Int] {

  override def parse(resource: String): Seq[String] =
    readResource(resource)

  override def part1(input: Seq[String]): Int = input
    .map(i => i.filter(_.isDigit))
    .map(i => "" + i.head + i.last)
    .map(_.toInt)
    .sum

  override def part2(input: Seq[String]): Int = input
    .map(i => transformSpelled(i))
    .map(i => i.filter(_.isDigit))
    .map(i => "" + i.head + i.last)
    .map(_.toInt)
    .sum

  @tailrec
  private def transformSpelled(input: String, index: Int = 0): String = {
    if(index >= input.length){
      return input
    }

    if(input.length >= index + 2 + 1 && input.substring(index, index + 3) == "one") transformSpelled(input.replaceFirst("one", "o1e"), index)
    else if(input.length >= index + 2 + 1 && input.substring(index, index + 3) == "two") transformSpelled(input.replaceFirst("two", "t2o"), index)
    else if(input.length >= index + 4 + 1 && input.substring(index, index + 5) == "three") transformSpelled(input.replaceFirst("three", "t3hree"), index)
    else if(input.length >= index + 3 + 1 && input.substring(index, index + 4) == "four") transformSpelled(input.replaceFirst("four", "f4ur"), index)
    else if(input.length >= index + 3 + 1 && input.substring(index, index + 4) == "five") transformSpelled(input.replaceFirst("five", "f5ive"), index)
    else if(input.length >= index + 2 + 1 && input.substring(index, index + 3) == "six") transformSpelled(input.replaceFirst("six", "s6ix"), index)
    else if(input.length >= index + 4 + 1 && input.substring(index, index + 5) == "seven") transformSpelled(input.replaceFirst("seven", "s7even"), index)
    else if(input.length >= index + 4 + 1 && input.substring(index, index + 5) == "eight") transformSpelled(input.replaceFirst("eight", "e8ight"), index)
    else if(input.length >= index + 3 + 1 && input.substring(index, index + 4) == "nine") transformSpelled(input.replaceFirst("nine", "n9ine"), index)

    else transformSpelled(input, index + 1)
  }

}
