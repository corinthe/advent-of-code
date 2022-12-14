package com.lmat.util

case class Node[+A](x: Int, y: Int, value: A)
case class DiWEdge[+A](source: Node[A], destination: Node[A], weight: Int)


case class Graph[+A](nodes: Seq[A], edges: DiWEdge[A]) {

}
case object Graph {

}
