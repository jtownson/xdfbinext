package net.jtownson.xdfbinext

object ReportExtractor {

  case class Note(lines: Vector[String])

  case class Acc(m: Map[String, String], currentTable: Option[String], currentNote: Option[Note]) {

    def startTable(t: String): Acc = Acc(m, Some(t), currentNote)

    def startNewNote: Acc =
      Acc(m, currentTable, Some(Note(Vector.empty)))

    def startNewNote(line: String): Acc =
      Acc(m, currentTable, Some(Note(Vector(line))))

    def addNoteLine(line: String): Acc =
      Acc(m, currentTable, currentNote.map(n => Note(n.lines :+ line)))

    def endNote: Acc =
      Acc(m + (currentTable.get -> currentNote.get.lines.mkString("\n")), currentTable = None, currentNote = None)

    val inNote: Boolean = currentNote.nonEmpty
  }

  def tables(report: Iterable[String]): List[String] = {
    report.flatMap { line =>
      val nl = line.trim
      if (nl.startsWith("Table (vector):") || nl.startsWith("Table (scalar):") || nl.startsWith("Table (matrix):")) {
        val tableName = nl.substring(15, nl.length).trim
        List(tableName)
      } else {
        List.empty
      }
    }.toList
  }

  def notes(report: Iterable[String]): Map[String, String] = {
    report
      .foldLeft(Acc(Map.empty, None, None)) { (acc, nextLine) =>
        val nl = nextLine.trim
        if (nl.startsWith("Table (vector):") || nl.startsWith("Table (scalar):") || nl.startsWith("Table (matrix):")) {
          val tableName = nl.substring(15, nl.length).trim
          acc.startTable(tableName)
        } else if (nl.startsWith("Notes:")) {
          require(acc.currentNote.isEmpty, "invalid start note state")
          val remainder = nl.substring(6, nl.length)
          if (remainder.nonEmpty)
            acc.startNewNote(remainder)
          else
            acc.startNewNote
        } else if (acc.inNote && nextLine.trim.nonEmpty) {
          acc.addNoteLine(nextLine.stripTrailing())
        } else if (acc.inNote && nextLine.trim.isEmpty) {
          acc.endNote
        } else {
          acc
        }
      }
      .m
  }
}
