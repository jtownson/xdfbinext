package net.jtownson.xdfbinext

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Using

object A2lXdfCategories {

  val table: Map[String, String] = Using.resource(Source.fromResource("a2l-object-xdf-cat.csv")) { src =>
    src.getLines.map { line =>
      val fields = parse(line, ',', '"')
      require(fields.length == 2, s"Invalid field count in line $line")
      val table       = fields(0).trim
      val description = fields(1).trim
      table -> description
    }.toMap
  }

  private val DEFAULT_SEPARATOR  = ','
  private val DOUBLE_QUOTES      = '"'
  private val DEFAULT_QUOTE_CHAR = DOUBLE_QUOTES
  private val NEW_LINE           = "\n"

  private def parse(line: String, separator: Char, quote: Char): List[String] = {

    val result                          = ArrayBuffer[String]()
    var isMultiLine                     = false
    var inQuotes                        = false
    var isFieldWithEmbeddedDoubleQuotes = false
    var pendingField                    = ""
    val field                           = new StringBuilder();

    line.toCharArray.foreach { c =>

      if (c == DOUBLE_QUOTES) { // handle embedded double quotes ""
        if (isFieldWithEmbeddedDoubleQuotes) {

          if (field.nonEmpty) { // handle for empty field like "",""
            field.append(DOUBLE_QUOTES);
            isFieldWithEmbeddedDoubleQuotes = false
          }
        } else {
          isFieldWithEmbeddedDoubleQuotes = true
        }
      } else {
        isFieldWithEmbeddedDoubleQuotes = false
      }

      if (isMultiLine) { // multiline, add pending from the previous field
        field.append(pendingField).append(NEW_LINE)
        pendingField = ""
        inQuotes = true
        isMultiLine = false
      }

      if (c == quote) {
        inQuotes = !inQuotes
      } else {
        if (c == separator && !inQuotes) { // if find separator and not in quotes, add field to the list
          result.addOne(field.toString())
          field.setLength(0) // empty the field and ready for the next
        } else {
          field.append(c) // else append the char into a field
        }
      }
    }

    // line done, what to do next?
    if (inQuotes) {
      pendingField = field.toString() // multiline
      isMultiLine = true
    } else {
      result.addOne(field.toString()) // this is the last field
    }

    result.toList
  }

  private def joinArrays(array1: Array[String], array2: Array[String]): Array[String] = {
    Array.concat(array1, array2)
  }
}
