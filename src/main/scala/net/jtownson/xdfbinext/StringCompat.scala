package net.jtownson.xdfbinext

import org.apache.commons.lang3.StringUtils

extension (s: String) {
  def stripTrailing(): String = {
    s.replaceFirst("\\s++$", "")
  }

  def repeat(n: Int): String = {
    (0 until n).map(_ => s).mkString
  }
}
