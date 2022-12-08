package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource
import com.lmat.util.StringExtensions.RichString

import scala.::

object Day05 extends SimpleCommonPuzzle[Seq[String], Int, Int] {



  override def parse(resource: String): Seq[String] =
    readResource(resource)

  override def part1(input: Seq[String]): Int = {
    def nice(str: String): Boolean = {
      val vowels = "aeiou"
      val forbidden = Seq("ab", "cd", "pq", "xy")
      val threeVowels: Boolean = str.count(vowels.contains(_)) >= 3
      val twiceInARow: Boolean = str.sliding(2).exists(s => s.head.equals(s.last))
      val noForbidden: Boolean = forbidden.forall(s => !str.contains(s))
      threeVowels && twiceInARow && noForbidden
    }

    input.count(nice)
  }

  override def part2(input: Seq[String]): Int = {
    def dropRepeated[A](seq: Seq[A]): Seq[A] = seq match {
      case Nil => Nil
      case x :: Nil => x :: Nil
      case x :: y :: Nil => x :: y :: Nil
      case x :: y :: xs =>
        if (x == y) x +: dropRepeated(xs)
        else x +: dropRepeated(y :: xs)
    }

    def nice(str: String): Boolean = {
      val repeatWithOneInBetween = str.sliding(3).exists(s => s.head.equals(s.last))
      val twoPairsNoOverlap = dropRepeated(str.sliding(2).toSeq).groupMapReduce(identity)(_ => 1)(_ + _).values.exists(_ >= 2)
      repeatWithOneInBetween && twoPairsNoOverlap
    }

    input.count(nice)
  }
}
