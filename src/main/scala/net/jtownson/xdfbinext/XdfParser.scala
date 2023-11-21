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
      tables = (xml \\ "XDFTABLE").map(node2Table(xdfHeader.categories))
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

  private def node2CategoryMem(categories: Seq[Category]): Node => CategoryMem = { n =>
    val index                = decode(n \@ "index")
    val actualZeroBasedIndex = decode(n \@ "category") - 1

    CategoryMem(index = index, category = categories(actualZeroBasedIndex))
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
      val dataType     = decode((xAxisNode \ "datatype").head.text)
      val unitType     = decode((xAxisNode \ "unittype").head.text)
      val embeddedData = node2EmbeddedData((xAxisNode \ "EMBEDDEDDATA").head)
      val daLink       = node2DaLink((xAxisNode \ "DALINK").head)
      val labels       = (xAxisNode \ "LABEL").map(node2Label)
      val math         = node2Math((xAxisNode \ "MATH").head)
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
      val dataType     = decode((yAxisNode \ "datatype").head.text)
      val unitType     = decode((yAxisNode \ "unittype").head.text)
      val embeddedData = node2EmbeddedData((yAxisNode \ "EMBEDDEDDATA").head)
      val daLink       = node2DaLink((yAxisNode \ "DALINK").head)
      val labels       = (yAxisNode \ "LABEL").map(node2Label)
      val math         = node2Math((yAxisNode \ "MATH").head)
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
      val decimalPl    = decode((zAxisNode \ "decimalpl").head.text)
      val min          = BigDecimal((zAxisNode \ "min").head.text)
      val max          = BigDecimal((zAxisNode \ "max").head.text)
      val outputType   = decode((zAxisNode \ "outputtype").head.text)
      val math         = node2Math((zAxisNode \ "MATH").head)
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

  private def node2Table(categories: Seq[Category]): Node => XdfTable = { n =>
    val uniqueId     = decode(n \@ "uniqueid")
    val flags        = decode(n \@ "flags")
    val title        = (n \ "title").head.text
    val description  = (n \ "description").headOption.map(_.text).getOrElse("")
    val categoryMems = (n \ "CATEGORYMEM").map(node2CategoryMem(categories))
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

  private def fail(msg: String): Nothing = throw new IllegalArgumentException(msg)
