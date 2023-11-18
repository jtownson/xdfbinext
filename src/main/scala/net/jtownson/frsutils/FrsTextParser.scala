package net.jtownson.frsutils

import fastparse.*
import SingleLineWhitespace.*

object FrsTextParser {

  case class Document(content: Seq[Content])

  case class SectionHeading(label: String, title: String)

  case class LinkingSectionHeading(label: String, link: String, title: String)

  case class PageNumber(page: String, of: String)

  case class Sentence(text: String)

  case class BulletPoint(label: String, text: String)

  case class StandaloneHeading(text: String)

  case class Linebreak()

  type Content = SectionHeading | PageNumber | Sentence | BulletPoint | StandaloneHeading | Linebreak |
    LinkingSectionHeading

  def standaloneHeading[T: P]: P[StandaloneHeading] = P(
    "" ~ CharsWhile(c => c != '\n').!.map(s => StandaloneHeading(s))
  )

  def linkingSectionHeading[T: P]: P[LinkingSectionHeading] = P(
    P(sectionLabel.! ~ linkBlock ~ sectionTitle.!).map((label, link, title) =>
      LinkingSectionHeading(label, link, title)
    )
  )

  private def linkBlock[T: P]: P[String] = P("[" ~ CharsWhile(c => c != ']').! ~ "]")

  def document[T: P]: P[Document] = documentStream.map(stream => Document(stream))

  def documentStream[T: P]: P[Seq[Content]] = P(content.rep(1))

  def content[T: P]: P[Content] = P(
    sectionHeading | linkingSectionHeading | pageNumber | standaloneHeading | linebreak /*| sentence | bulletPoint*/
  )

  def linebreak[T: P]: P[Linebreak] = P("\n").map(_ => Linebreak())
  def sectionHeading[T: P]: P[SectionHeading] =
    P(sectionLabel.! ~ sectionTitle.!).map((label, title) => SectionHeading(label, title))

  private def sectionLabel[T: P]: P[String] = P((romanLabel | numericLabel).! ~ !"|")

  private def numericLabel[T: P]: P[String] = P((CharIn("0-9").rep(1) ~ ".").? ~ CharIn("0-9").rep(1)).!

  private def romanLabel[T: P]: P[String] = P(StringIn("I", "II", "I:", "II:").!)

  private def sectionTitle[T: P]: P[String] = P(!"[" ~ CharsWhile(c => c != '\n').!)

  def pageNumber[T: P]: P[PageNumber] =
    P(CharIn("0-9").rep(1).! ~ "|" ~ CharIn("0-9").rep(1).!).map((l, r) => PageNumber(l, r))

  def sentence[T: P]: P[Sentence] = P(CharsWhile(c => c != '.').!.map(s => Sentence(s)))

  def parseDocument(content: String): Parsed[Document] = {
    fastparse.parse(content, document(_))
  }
}
