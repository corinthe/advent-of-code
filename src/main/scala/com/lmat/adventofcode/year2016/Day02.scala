package com.lmat.adventofcode.year2016

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2016.Day02Definitions.{Button, Down, Five, Left, Mouvement, Right, Up}
import com.lmat.util.Files.readResource

object Day02Definitions {

  trait Mouvement

  sealed class Button(horizontal: Int, vertical: Int) {
    def exec(mov: Mouvement): Button = mov match {
      case Up if this.horizontal > 0 => toButton((this.horizontal - 1) * 3 + (this.vertical + 1))
      case Down if this.horizontal < 2 => toButton((this.horizontal + 1) * 3 + (this.vertical + 1))
      case Left if this.vertical > 0 => toButton(this.horizontal * 3 + this.vertical)
      case Right if this.vertical < 2 => toButton(this.horizontal * 3 + (this.vertical + 2))
      case _ => this
    }

    def toNum: Int = (horizontal * 3) + (vertical + 1)

    def toButton(num: Int): Button = num match {
      case 1 => One
      case 2 => Two
      case 3 => Three
      case 4 => Four
      case 5 => Five
      case 6 => Six
      case 7 => Seven
      case 8 => Eight
      case 9 => Nine
    }

  }

  case object Up extends Mouvement

  case object Down extends Mouvement

  case object Left extends Mouvement

  case object Right extends Mouvement

  case object One extends Button(0, 0)

  case object Two extends Button(0, 1)

  case object Three extends Button(0, 2)

  case object Four extends Button(1, 0)

  case object Five extends Button(1, 1)

  case object Six extends Button(1, 2)

  case object Seven extends Button(2, 0)

  case object Eight extends Button(2, 1)

  case object Nine extends Button(2, 2)
}

object Day02 extends SimpleCommonPuzzle[Seq[Seq[Mouvement]], String, Int] {


  override def parse(resource: String): Seq[Seq[Mouvement]] =
    readResource(resource).map(parseLine)

  def parseLine(line: String): Seq[Mouvement] =
    line.split("").map {
      case "U" => Up
      case "D" => Down
      case "L" => Left
      case "R" => Right
    }

  override def part1(input: Seq[Seq[Mouvement]]): String = {
    input.scanLeft(Five.asInstanceOf[Button])((button, moves) => moves.foldLeft(button)(_.exec(_))).tail.map(_.toNum).toString()
  }

  override def part2(input: Seq[Seq[Mouvement]]): Int = 0
}
