package net.jtownson.xdfbinext

object XdfSchema:

  case class XdfModel(version: String, xdfHeader: XdfHeader, tables: Seq[XdfTable]) {
    def tablesByName: Map[String, XdfTable] = tables.map(t => t.title -> t).toMap
  }

  case class XdfHeader(
      flags: Int,
      description: String,
      baseOffset: BaseOffset,
      defaults: Defaults,
      region: Region,
      categories: Seq[Category]
  )

  case class BaseOffset(offset: Int, subtract: Int)

  case class Defaults(
      dataSizeInBits: Int,
      sigDigits: Int,
      outputType: Int,
      signed: Int,
      lsbfirst: Int,
      float: Int
  )

  case class Region(
      `type`: Long,
      startAddress: Long,
      size: Long,
      regionColor: Int,
      regionFlags: Int,
      name: String,
      desc: String
  )

  case class Category(index: Int, name: String)

  case class XdfTable(
      uniqueId: Int,
      flags: Int,
      title: String,
      categoryMems: Seq[CategoryMem],
      axes: Axes
  )

  case class Axes(x: XdfAxisX, y: XdfAxisY, z: XdfAxisZ)

  case class CategoryMem(index: Int, category: Int)

  case class XdfAxisX(
      id: String,
      uniqueId: Int,
      embeddedData: EmbeddedData,
      indexCount: Int,
      dataType: Int,
      unitType: Int,
      daLink: DaLink,
      labels: Seq[Label],
      math: Math
  )

  case class XdfAxisY(
      id: String,
      uniqueId: Int,
      embeddedData: EmbeddedData,
      indexCount: Int,
      dataType: Int,
      unitType: Int,
      daLink: DaLink,
      labels: Seq[Label],
      math: Math
  )

  case class XdfAxisZ(
      id: String,
      embeddedData: EmbeddedData,
      decimalPl: Int,
      min: BigDecimal,
      max: BigDecimal,
      outputType: Int,
      math: Math
  )

  case class EmbeddedData(
      mmedTypeFlags: Int,
      mmedAddress: Long,
      mmedElementSizeBits: Int,
      mmedRowCount: Int,
      mmedColCount: Int,
      mmedMajorStrideBits: Int,
      mmedMinorStrideBits: Int
  )

  case class DaLink(index: Int)

  case class Label(index: Int, value: String)

  case class Math(equation: String, vars: Seq[Var])

  case class Var(id: String)
