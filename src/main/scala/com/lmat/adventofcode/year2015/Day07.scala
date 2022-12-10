package com.lmat.adventofcode.year2015

import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource
import scalax.collection.GraphEdge.{DiEdge}
import scalax.collection.GraphPredef.EdgeAssoc
import scalax.collection.mutable.Graph

object Day07Definitions {
  type Signal = Int
  type Wire = String

  trait Instruction

  case class Provide(signal: Signal, wire: String) extends Instruction
  case class And(input1: Wire, input2: Wire, output: Wire) extends Instruction
  case class Or(input1: Wire, input2: Wire, output: Wire) extends Instruction
  case class Not(input: Wire, output: Wire) extends Instruction
  case class Lshift(input: Wire, amount: Int,  output: Wire) extends Instruction
  case class Rshift(input: Wire, amount: Int,  output: Wire) extends Instruction

  case class Node(name: Wire) {
    var operation: Option[Op] = Option.empty

    def op(): Op = op
  }

  trait Op

  case class Value(v: Int) extends Op
  case class Variable(name: Wire) extends Op
  case class NotOp(name: Wire) extends Op
  case class AndOp(name: Wire) extends Op
  case class OrOp(name: Wire) extends Op
  case class LshiftOp(amount: Int) extends Op
  case class RshiftOp(amount: Int) extends Op

  case object Circuit {
    val graph: Graph[Node, DiEdge] = Graph.empty

    def valueOf(n: Node): Int = n.op() match {
      case Value(v) => v
      case Variable(_) => valueOf(graph.get(n).diPredecessors.head)
      case NotOp(_) => ~ valueOf(graph.get(n).diPredecessors.head)
      case AndOp(_) => graph.get(n).diPredecessors.map(x => this.valueOf(x)).reduceLeft(_ & _)
      case OrOp(_) => graph.get(n).diPredecessors.map(x => this.valueOf(x)).reduceLeft(_ | _)
      case LshiftOp(amount) => valueOf(graph.get(n).diPredecessors.head) << amount
      case RshiftOp(amount) => valueOf(graph.get(n).diPredecessors.head) >> amount
      case _ => ???
    }
  }
}

object Day07 extends SimpleCommonPuzzle[Seq[String], Int, Int] {

  override def parse(resource: String): Seq[String] =
    readResource(resource)

  override def part1(input: Seq[String]): Int = ???

  override def part2(input: Seq[String]): Int = ???
}
