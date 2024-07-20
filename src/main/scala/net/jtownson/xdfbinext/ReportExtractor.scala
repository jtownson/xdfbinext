package net.jtownson.xdfbinext

object ReportExtractor {

  case class Note(lines: Vector[String])

  case class Acc(m: Map[String, String], currentTable: Option[String], currentNote: Option[Note]) {

    def startTable(t: String): Acc = Acc(m, Some(t), currentNote)

    def startNewNote: Acc =
      currentTable.fold(this)(_ => Acc(m, currentTable, Some(Note(Vector.empty))))

    def startNewNote(line: String): Acc =
      currentTable.fold(this)(_ => Acc(m, currentTable, Some(Note(Vector(line)))))

    def addNoteLine(line: String): Acc =
      Acc(m, currentTable, currentNote.map(n => Note(n.lines :+ line)))

    def endNote: Acc =
      currentTable.fold(this)(t => Acc(m + (t -> trimNote.mkString("\n")), currentTable = None, currentNote = None))

    private def trimNote: Vector[String] =
      currentNote.get.lines.dropWhile(_.isBlank).reverse.dropWhile(_.isBlank).reverse

    val inNote: Boolean = currentNote.nonEmpty
  }

  def tables(report: Iterable[String]): List[String] = {
    report.flatMap { line =>
      val nl      = line.trim
      val mbTable = tableRegex.findFirstMatchIn(nl)
      if (mbTable.nonEmpty) {
        val tableName = mbTable.get.group(1).trim
        List(tableName)
      } else {
        List.empty
      }
    }.toList
  }

  private val tableRegex   = """===\s(.+)\s===""".r
  private val notesRegex   = """'''Notes''':""".r
  private val wikiSegRegex = """'''.+''':""".r

  def notes(report: Iterable[String]): Map[String, String] = {
    report
      .foldLeft(Acc(Map.empty, None, None)) { (acc, nextLine) =>
        val nl      = nextLine.trim
        val mbTable = tableRegex.findFirstMatchIn(nl)
        if (mbTable.nonEmpty) {
          val tableName = mbTable.get.group(1).trim
          if (acc.inNote)
            acc.endNote.startTable(tableName)
          else
            acc.startTable(tableName)
        } else if (nl.startsWith("'''Notes''':")) {
          require(acc.currentNote.isEmpty, "invalid start note state")
          val remainder = nl.substring(12, nl.length)
          if (remainder.nonEmpty)
            acc.startNewNote(remainder)
          else
            acc.startNewNote
        } else if (acc.inNote && !nextLine.isBlank) {
          acc.addNoteLine(nextLine.stripTrailing())
        } else if (acc.inNote && (wikiSegRegex.matches(nl) || tableRegex.matches(nl))) {
          acc.endNote
        } else if (acc.inNote && nextLine.isBlank) {
          acc.addNoteLine("")
        } else {
          acc
        }
      }
      .m
  }
}
