package com.lmat.util

case class Matrix[+A](rows: Vector[Vector[A]]) {
  def height: Int = rows.size

  def width: Int = if (rows.isEmpty) 0 else rows.head.size

  def columns: Vector[Vector[A]] = (for {
    column <- 0 until width
  } yield rows.map(row => row(column))).toVector

  def map[B](f: A => B): Matrix[B] = Matrix(rows.map(_.map(f)))

  override def toString: String = rows.map(_.mkString(" ")).mkString("\n")

  def findAllPositions[B >: A](elem: B): Seq[(Int, Int)] = {
    for {
      (row, rowIndex) <- rows.zipWithIndex
      (value, colIndex) <- row.zipWithIndex
      if value == elem
    } yield (rowIndex, colIndex)
  }

  private val directions = Seq(
    (0, 1), // Right
    (0, -1), // Left
    (1, 0), // Down
    (-1, 0), // Up
    (1, 1), // Diagonal Down-Right
    (-1, -1), // Diagonal Up-Left
    (1, -1), // Diagonal Down-Left
    (-1, 1) // Diagonal Up-Right
  )

  def generateSequencesFromPositions(positions: Seq[(Int, Int)], length: Int): Seq[Seq[A]] = {

    val numRows = rows.length
    val numCols = if (rows.isEmpty) 0 else rows.head.length

    def isValidPosition(x: Int, y: Int): Boolean =
      x >= 0 && y >= 0 && x < numRows && y < numCols

    def sequenceFromPosition(x: Int, y: Int, dx: Int, dy: Int): Option[Seq[A]] = {
      val seq = for (i <- 0 until length if isValidPosition(x + i * dx, y + i * dy))
        yield rows(x + i * dx)(y + i * dy)
      if (seq.length == length) Some(seq) else None
    }

    for {
      (startX, startY) <- positions
      (dx, dy) <- directions
      seq <- sequenceFromPosition(startX, startY, dx, dy)
    } yield seq
  }
}

object Matrix {
  val empty: Matrix[Nothing] = Matrix(Vector(Vector()))

  def ofSize[A](s: Int, elem: A) = Matrix(Vector.fill(s)(Vector.fill(s)(elem)))

  def flipHorizontal[A](matrix: Matrix[A]): Matrix[A] = Matrix(matrix.rows.map(_.reverse))

  def flipVertical[A](matrix: Matrix[A]): Matrix[A] = Matrix(matrix.rows.reverse)

  def rotateLeft[A](matrix: Matrix[A]): Matrix[A] = flipVertical(Matrix(matrix.columns))

  def rotateRight[A](matrix: Matrix[A]): Matrix[A] = flipHorizontal(Matrix(matrix.columns))

  def breakHorizontal[A](matrix: Matrix[A], c: Int): Vector[Matrix[A]] =
    if (matrix.width <= c) Vector(matrix)
    else {
      val matrices = for {
        p <- 0 until matrix.width by c
      } yield Matrix(matrix.rows.map(_.slice(p, p + c)))
      matrices.toVector
    }

  def breakVertical[A](matrix: Matrix[A], r: Int): Vector[Matrix[A]] =
    if (matrix.height <= r) Vector(matrix)
    else {
      val matrices = for {
        p <- 0 until matrix.height by r
      } yield Matrix(matrix.rows.slice(p, p + r))
      matrices.toVector
    }

  def break[A](matrix: Matrix[A], r: Int, c: Int): Matrix[Matrix[A]] =
    Matrix(breakVertical(matrix, r).map(breakHorizontal(_, c)))

  def concatHorizontal[A](matrix1: Matrix[A], matrix2: Matrix[A]): Matrix[A] =
    Matrix((matrix1.rows zip matrix2.rows).map { case (v1, v2) => v1 ++ v2 })

  def concatHorizontal[A](matrices: Vector[Matrix[A]]): Matrix[A] =
    if (matrices.isEmpty) empty else matrices.reduce(concatHorizontal[A])

  def concatVertical[A](matrix1: Matrix[A], matrix2: Matrix[A]): Matrix[A] =
    Matrix(matrix1.rows ++ matrix2.rows)

  def concatVertical[A](matrices: Vector[Matrix[A]]): Matrix[A] =
    if (matrices.isEmpty) empty else matrices.reduce(concatVertical[A])

  def merge[A](matrices: Matrix[Matrix[A]]): Matrix[A] =
    concatVertical[A](matrices.rows.map(concatHorizontal[A]))

  def lefts[A](matrix: Matrix[A], r: Int, c: Int): Vector[A] =
    matrix.rows(r).take(c)

  def rights[A](matrix: Matrix[A], r: Int, c: Int): Vector[A] =
    matrix.rows(r).drop(c + 1)

  def ups[A](matrix: Matrix[A], r: Int, c: Int): Vector[A] =
    matrix.columns(c).take(r)

  def downs[A](matrix: Matrix[A], r: Int, c: Int): Vector[A] =
    matrix.columns(c).drop(r + 1)
}
