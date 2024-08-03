package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.XdfSchema.{Region, XdfTable, *}

import java.lang.Integer.decode
import java.lang.Long.decode as decodeLong
import scala.xml.{Node, XML}

object XdfParser:

  def parse(xdf: String): XdfModel =
    parse(XML.loadString(xdf))

  private def parse(xml: scala.xml.Elem): XdfModel = {
    val xdfHeader = node2Header((xml \\ "XDFFORMAT" \ "XDFHEADER").head)
    XdfModel(
      version = xml \\ "XDFFORMAT" \@ "version",
      xdfHeader = xdfHeader,
      tables = (xml \\ "XDFTABLE").map(node2Table(xdfHeader)),
      virtualTables = (xml \\ "VIRTUALTABLE").map(node2VirtualTable)
    )
  }

  private def optionalInt(n: Node, xpath: String, default: Int): Int =
    (n \ xpath).headOption.map(n => decode(n.text).toInt).getOrElse(default)

  private def optionalLong(n: Node, xpath: String, default: Long): Long =
    (n \ xpath).headOption.map(n => decodeLong(n.text).toLong).getOrElse(default)

  private val node2Category: Node => Category = { n =>
    val index = n \@ "index"
    val name  = n \@ "name"
    Category(index, name)
  }

  private def node2CategoryMem(header: XdfHeader): Node => CategoryMem = { n =>
    val index    = decode(n \@ "index")
    val indexRef = decode(n \@ "category") - 1

    CategoryMem(index = index, category = header.categoryIndex(indexRef))
  }

  private val node2EmbeddedData: Node => EmbeddedData = { n =>
    val mmedTypeFlags = optionalInt(n, "@mmedtypeflags", 0)
    val mmedAddress   = optionalLong(n, "@mmedaddress", undefinedAddress)
    val mmedRowCount  = optionalInt(n, "@mmedrowcount", 0)
    val mmedColCount  = optionalInt(n, "@mmedcolcount", 0)
    EmbeddedData(
      mmedTypeFlags = mmedTypeFlags,
      mmedAddress = mmedAddress,
      mmedElementSizeBits = decode(n \@ "mmedelementsizebits"),
      mmedRowCount = mmedRowCount,
      mmedColCount = mmedColCount,
      mmedMajorStrideBits = decode(n \@ "mmedmajorstridebits"),
      mmedMinorStrideBits = decode(n \@ "mmedminorstridebits")
    )
  }

  private val node2Alias: Node => (String, String) = { n =>
    val label = (n \@ "label")
    val table = (n \@ "table")
    (label, table)
  }

  private val node2ExpressionDef: Node => XdfExpression = { n =>
    val expressionValue = (n \@ "value")
    val aliases         = (n \ "alias").map(node2Alias).toMap
    XdfExpression(aliases, expressionValue)
  }

  private val node2InverseLookup: Node => InverseLookup2D = { n =>
    val table  = n \@ "table"
    val invert = n \@ "invert"

    InverseLookup2D(table, invert)
  }

  private val node2DaLink: Node => DaLink = { n => DaLink(index = decode(n \@ "index")) }

  private val node2Var: Node => Var = n => Var(id = n \@ "id")

  private val node2Math: Node => Math = { n =>
    val equation = n \@ "equation"
    val vars     = (n \ "VAR").map(node2Var)
    Math(equation = equation, vars = vars)
  }

  private val node2Label: Node => Label = { n =>
    val index = decode(n \@ "index")
    val value = n \@ "value"
    Label(index = index, value = value)
  }

  private val node2Axes: Node => Axes = { n =>
    val axisNodes = n \ "XDFAXIS"

    def axis(id: String): Node => Boolean = n => (n \@ "id") == id

    val xAxisNode = axisNodes.find(axis("x")).getOrElse(fail("missing x axis in $n"))
    val xAxis = {
      val uniqueId     = decode(n \@ "uniqueid")
      val indexCount   = decode((xAxisNode \ "indexcount").head.text)
      val dataType     = (xAxisNode \ "datatype").headOption.map(_.text).map(decode).map(_.toInt)
      val unitType     = (xAxisNode \ "unittype").headOption.map(_.text).map(decode).map(_.toInt)
      val embeddedData = node2EmbeddedData((xAxisNode \ "EMBEDDEDDATA").head)
      val daLink       = (xAxisNode \ "DALINK").headOption.map(node2DaLink)
      val labels       = (xAxisNode \ "LABEL").map(node2Label)
      val math         = (xAxisNode \ "MATH").headOption.map(node2Math)
      val units        = (xAxisNode \ "units").headOption.map(_.text).getOrElse("")

      XdfAxisX(
        id = "x",
        uniqueId = uniqueId,
        embeddedData = embeddedData,
        indexCount = indexCount,
        dataType = dataType,
        unitType = unitType,
        daLink = daLink,
        labels = labels,
        math = math,
        units = units
      )
    }

    val yAxisNode = axisNodes.find(axis("y")).getOrElse(fail("missing y axis in $n"))
    val yAxis = {
      val uniqueId     = decode(n \@ "uniqueid")
      val indexCount   = decode((yAxisNode \ "indexcount").head.text)
      val dataType     = (yAxisNode \ "datatype").headOption.map(_.text).map(decode).map(_.toInt)
      val unitType     = (yAxisNode \ "unittype").headOption.map(_.text).map(decode).map(_.toInt)
      val embeddedData = node2EmbeddedData((yAxisNode \ "EMBEDDEDDATA").head)
      val daLink       = (yAxisNode \ "DALINK").headOption.map(node2DaLink)
      val labels       = (yAxisNode \ "LABEL").map(node2Label)
      val math         = (yAxisNode \ "MATH").headOption.map(node2Math)
      val units        = (yAxisNode \ "units").headOption.map(_.text).getOrElse("")

      XdfAxisY(
        id = "y",
        uniqueId = uniqueId,
        embeddedData = embeddedData,
        indexCount = indexCount,
        dataType = dataType,
        unitType = unitType,
        daLink = daLink,
        labels = labels,
        math = math,
        units = units
      )
    }

    val zAxisNode = axisNodes.find(axis("z")).getOrElse(fail("missing z axis in $n"))
    val zAxis = {
      val embeddedData = node2EmbeddedData((zAxisNode \ "EMBEDDEDDATA").head)
      val decimalPl    = (zAxisNode \ "decimalpl").headOption.map(_.text).map(_.toInt)
      val min          = (zAxisNode \ "min").headOption.map(_.text).map(BigDecimal(_))
      val max          = (zAxisNode \ "max").headOption.map(_.text).map(BigDecimal(_))
      val outputType   = (zAxisNode \ "outputtype").headOption.map(_.text).map(decode).map(_.toInt)
      val math         = (zAxisNode \ "MATH").headOption.map(node2Math)
      val units        = (zAxisNode \ "units").headOption.map(_.text).getOrElse("")

      XdfAxisZ(
        id = "z",
        embeddedData = embeddedData,
        decimalPl = decimalPl,
        min = min,
        max = max,
        outputType = outputType,
        math = math,
        units = units
      )
    }

    Axes(xAxis, yAxis, zAxis)
  }

  private def node2VirtualTable: Node => XdfVirtualTable = { n =>
    val title                 = (n \ "title").head.text
    val description           = (n \ "description").headOption.map(_.text).getOrElse("")
    val maybeInverseLookupDef = (n \ "INVERSELOOKUP").headOption.map(node2InverseLookup)
    val maybeExpressionDef    = (n \ "EXPRESSION").headOption.map(node2ExpressionDef)

    val tableDef = maybeInverseLookupDef
      .orElse(maybeExpressionDef)
      .getOrElse(
        throw new IllegalArgumentException(
          s"Invalid virtual table definition $n. Expected either INVERSELOOKUP or EXPRESSION."
        )
      )

    XdfVirtualTable(title, description, tableDef)
  }

  private def node2Table(header: XdfHeader): Node => XdfTable = { n =>
    val uniqueId     = decode(n \@ "uniqueid")
    val flags        = decode(n \@ "flags")
    val title        = (n \ "title").head.text
    val description  = (n \ "description").headOption.map(_.text).getOrElse("")
    val categoryMems = (n \ "CATEGORYMEM").map(node2CategoryMem(header))
    val axes         = node2Axes(n)

    XdfTable(
      uniqueId = uniqueId,
      flags = flags,
      title = title,
      description = description,
      categoryMems = categoryMems,
      axes = axes
    )
  }

  private val node2Header: Node => XdfHeader = { header =>
    val baseOffset = header \ "BASEOFFSET"
    val defaults   = header \ "DEFAULTS"
    val region     = header \ "REGION"

    XdfHeader(
      flags = decode((header \ "flags").head.text),
      description = (header \ "description").text,
      baseOffset = BaseOffset(
        offset = decode(baseOffset \@ "offset"),
        subtract = decode(baseOffset \@ "subtract")
      ),
      defaults = Defaults(
        dataSizeInBits = decode(defaults \@ "datasizeinbits"),
        sigDigits = decode(defaults \@ "sigdigits"),
        outputType = decode(defaults \@ "outputtype"),
        signed = decode(defaults \@ "signed"),
        lsbfirst = decode(defaults \@ "lsbfirst"),
        float = decode(defaults \@ "float")
      ),
      region = Region(
        `type` = decodeLong(region \@ "type"),
        startAddress = decodeLong(region \@ "startaddress"),
        size = decodeLong(region \@ "size"),
        regionColor = decode(region \@ "regioncolor"),
        regionFlags = decode(region \@ "regionflags"),
        name = region \@ "name",
        desc = region \@ "desc"
      ),
      categories = (header \ "CATEGORY").map(node2Category)
    )
  }

  private def decode(s: String): Integer = {
    if (s.trim.isEmpty)
      0
    else Integer.decode(s)
  }

  private def fail(msg: String): Nothing = throw new IllegalArgumentException(msg)
