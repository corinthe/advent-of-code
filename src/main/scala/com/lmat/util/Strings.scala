package com.lmat.util

import java.security.MessageDigest

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

object StringExtensions {
  implicit class RichString(val str: String) extends AnyVal {
    def md5: String = MessageDigest.getInstance("MD5").digest(str.getBytes).map("%02X".format(_)).mkString

    def nice: Boolean = {
      val vowels = "aeiou"
      val forbidden = Seq("ab", "cd", "pq", "xy")
      val threeVowels: Boolean = str.count(vowels.contains(_)) >= 3
      val twiceInARow: Boolean = str.sliding(2).exists(s => s.head.equals(s.last))
      val noForbidden: Boolean = forbidden.forall(s => !str.contains(s))
      threeVowels && twiceInARow && noForbidden
    }
  }
}
