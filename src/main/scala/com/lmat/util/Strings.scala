package com.lmat.util

object Strings {
  /**
    * Left pad a string to a length with the specified character
    */
  def leftPad(source: String)(pad: Char, length: Int): String =
    (pad.toString * (length - source.length)) + source

  def rightPad(source: String)(pad: Char, length: Int): String =
    source + (pad.toString * (length - source.length))

  /**
    * Find all indexesOf the query string in source
    * Return both the starting (inclusive) and ending indices (non-inclusive)
    */
  def indicesOf(raw: String, query: String): Seq[(Int, Int)] =
    raw.scanRight("")((c, s) => s"$c$s").dropRight(1).zipWithIndex
      .filter { case (test, _)     => test.startsWith(query) }
      .map    { case (_,    index) => (index, index + query.length) }
}
