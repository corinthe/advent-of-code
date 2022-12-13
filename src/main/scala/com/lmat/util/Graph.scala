package com.lmat.util

case class Node[+A](x: Int, y: Int, value: A)
case class DiWEdge[+A](source: Node[A], destination: Node[A], weight: Int)


case class Graph[+A](nodes: Seq[A], edges: DiWEdge[A]) {

}
case object Graph {

  def fromMatrix[A](m: Matrix[A]): Graph[A] = {
    val nodes: Seq[Node[A]] = m.rows.zipWithIndex.flatMap { case (row, i) =>
      row.zipWithIndex.map { case (value, j) =>
        Node(i, j, value)
      }
    }

    val edges: Seq[DiWEdge[A]] = m.rows.zipWithIndex.flatMap { case (row, i) =>
      row.zipWithIndex.flatMap { case (value, j) =>
        // Add an edge to the right
        if (j < m.rows(i).length - 1) {
          val src = Node(i, j, value)
          val dest = Node(i, j + 1, m.rows(i)(j + 1))
          Some(DiWEdge(src, dest, m.rows(i)(j + 1) - value))
        }

        // Add an edge to the bottom
        if (i < m.rows.length - 1) {
          val src = Node(i, j, value)
          val dest = Node(i + 1, j, m.rows(i + 1)(j))
          Some(DiWEdge(src, dest, m.rows(i + 1)(j) - value))
        } else {
          None
        }
      }
    }
  }

}
