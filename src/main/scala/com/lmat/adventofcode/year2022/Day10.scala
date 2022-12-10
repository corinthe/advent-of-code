package com.lmat.adventofcode.year2022

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.adventofcode.year2022.Day10Definitions.{Add, Instruction, Noop, Program}
import com.lmat.util.Files.readResource

object Day10Definitions {

  type PixelPos = Int
  type Tick = Int

  trait Instruction {
    def unfold: Seq[Instruction]
  }

  case class Add(v: Int) extends Instruction {
    override def unfold: Seq[Instruction] = Seq(Wait, Add(v))
  }
  case object Noop extends Instruction {
    override def unfold: Seq[Instruction] = Seq(Noop)
  }
  case object Wait extends Instruction {
    override def unfold: Seq[Instruction] = Seq(Wait)
  }

  case class CPU(registerX: Int) {

    def execute(i: Instruction): CPU = i match {
      case Add(v) => CPU(registerX + v)
      case Noop => CPU(registerX)
      case Wait => CPU(registerX)
      case _ => throw new IllegalStateException()
    }
  }

  case class Program(instructions: Seq[Instruction]) {

    private val unfoldedInstructions = instructions.flatMap(_.unfold)
    private val importantTicks = Seq(20, 60, 100, 140, 180, 220)
    private val ticks = LazyList.from(1)

    private val screenLength = 40
    private val screenHeight = 6

    private val registers = unfoldedInstructions.scanLeft(CPU(1))(_.execute(_)).map(_.registerX)
    private val signalStrengths: Seq[Int] = registers.zip(ticks).map {case (register, cycle) => register * cycle}

    def solve1(): Int = {
      importantTicks.map(c => signalStrengths(c - 1)).sum
    }

    def solve2(): Unit = {
      printScreen(registers
        .zip(ticks)
        .foldLeft(Array.fill(screenLength * screenHeight)(false)){ case (screen, (register, tick)) =>
          if(tick - 1 < screen.length) screen(tick - 1) = spriteVisible(register, tick)
          screen
        })
    }

    private def pixelsCovered(spritePos: PixelPos): Seq[PixelPos] = Seq(spritePos - 1, spritePos, spritePos + 1)
    private def spriteVisible(register: Int, tick: Tick): Boolean = pixelsCovered(register).contains((tick - 1) % screenLength)

    private def printScreen(screen: Array[Boolean]): Unit = println(screen.toSeq.map(v => if(v) '#' else '.').mkString("").grouped(screenLength).toSeq.mkString("\n"))
  }
}

object Day10 extends SimpleCommonPuzzle[Program, Int, Int] {

  def parseLine(s: String): Instruction = s match {
    case "noop" => Noop
    case _ => Add(s.split(" ").last.toInt)
  }

  override def parse(resource: String): Program =
    Program(readResource(resource).map(parseLine))

  override def part1(input: Program): Int = input.solve1()

  override def part2(input: Program): Int = {
    input.solve2()
    0
  }
}
