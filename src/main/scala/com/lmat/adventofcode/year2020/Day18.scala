package com.lmat.adventofcode.year2020

import atto.Atto.{bigInt, char, int, ok, toParserOps}
import atto.Parser
import com.lmat.adventofcode.SimpleCommonPuzzle
import com.lmat.util.Files.readResource

object Day18 extends SimpleCommonPuzzle[Seq[String], BigInt, BigInt] {

  lazy val Expr1: Parser[BigInt] = for {
    lhs  <- Factor1
    next <- Expr_prim1
  } yield next(lhs)

  lazy val Expr_prim1: Parser[BigInt => BigInt] = {
    val p1 = for {
      _    <- char('+')
      rhs  <- Factor1
      next <- Expr_prim1
    } yield (lhs: BigInt) => next(lhs + rhs)
    val p2 = for {
      _    <- char('*')
      rhs  <- Factor1
      next <- Expr_prim1
    } yield (lhs: BigInt) => next(lhs * rhs)
    val p3 = ok((lhs: BigInt) => lhs)
    p1 | p2 | p3
  }

  lazy val Factor1: Parser[BigInt] = {
    val p1 = for {
      _ <- char('(')
      e <- Expr1
      _ <- char(')')
    } yield e
    val p2 = bigInt
    p1 | p2
  }

  lazy val Expr2: Parser[BigInt] = for {
    lhs  <- Term2
    next <- Expr_prim2
  } yield next(lhs)

  lazy val Expr_prim2: Parser[BigInt => BigInt] = {
    val p1 = for {
      _    <- char('*')
      rhs  <- Term2
      next <- Expr_prim2
    } yield (lhs: BigInt) => next(lhs * rhs)
    val p3 = ok((lhs: BigInt) => lhs)
    p1 | p3
  }

  lazy val Term2: Parser[BigInt] = for {
    lhs  <- Factor2
    next <- Term_prim2
  } yield next(lhs)

  lazy val Term_prim2: Parser[BigInt => BigInt] = {
    val p1 = for {
      _    <- char('+')
      rhs  <- Factor2
      next <- Term_prim2
    } yield (lhs: BigInt) => next(lhs + rhs)
    val p3 = ok((lhs: BigInt) => lhs)
    p1 | p3
  }

  lazy val Factor2: Parser[BigInt] = {
    val p1 = for {
      _ <- char('(')
      e <- Expr2
      _ <- char(')')
    } yield e
    val p2 = bigInt
    p1 | p2
  }

  override def parse(resource: String): Seq[String] =
    readResource(resource).map(_.replace(" ", ""))

  override def part1(input: Seq[String]): BigInt = {
    input.flatMap(l => Expr1.parse(l).done.option).sum
  }

  override def part2(input: Seq[String]): BigInt =
    input.flatMap(l => Expr2.parse(l).done.option).sum
}
