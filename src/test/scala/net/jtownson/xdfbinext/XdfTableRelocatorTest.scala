package net.jtownson.xdfbinext

import net.jtownson.xdfbinext.XdfSchema.*
import net.jtownson.xdfbinext.XdfTableRelocator.RelocationCandidate
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import java.io.File
import java.nio.file.Files
import scala.io.Source

class XdfTableRelocatorTest extends AnyFlatSpec {

  "XdfTableRelocator" should "return the address of the table for the same bin" in {
    val xdfModel = XdfParser.parse(Source.fromResource("00003076501103.xdf").mkString)

    val mapSwitchBaseBin = new File(
      getClass.getClassLoader.getResource("00003076501103_MapSwitchBase.bin").toURI
    )

    val originalBin = new File(
      getClass.getClassLoader.getResource("00003076501103_original.bin").toURI
    )

    val binAdapter = new XDFBinAdapter(mapSwitchBaseBin, xdfModel)

    val relocator = new XdfTableRelocator(binAdapter)

    val candidates = relocator.relocate(
      tableName = "Performance gauge scaling",
      targetBin = originalBin,
      topN = 3
    )

    candidates.head shouldBe RelocationCandidate("Performance gauge scaling", address = 0x68A554, variance = 0.0)

    candidates.foreach(println)
  }

  "XdfTableRelocator" should "find the relocated table address by minimum variance" in {
    val tableName      = "T"
    val sourceAddress  = 5L
    val relocatedTo    = 20L
    val sourceBytes    = Array[Byte](10, 20, 30, 40)
    val sourceBinBytes = Array.fill[Byte](64)(0)
    val targetBinBytes = Array.fill[Byte](64)(0)

    Array.copy(sourceBytes, 0, sourceBinBytes, sourceAddress.toInt, sourceBytes.length)
    Array.copy(sourceBytes, 0, targetBinBytes, relocatedTo.toInt, sourceBytes.length)

    val sourcePath = Files.createTempFile("source-xdf-relocator", ".bin")
    val targetPath = Files.createTempFile("target-xdf-relocator", ".bin")
    Files.write(sourcePath, sourceBinBytes)
    Files.write(targetPath, targetBinBytes)

    val model      = simpleModel(tableName, sourceAddress, count = sourceBytes.length)
    val sourceAdpt = new XDFBinAdapter(sourcePath.toFile, model)
    val relocator  = new XdfTableRelocator(sourceAdpt)

    val candidates = relocator.relocate(
      tableName = tableName,
      targetBin = targetPath.toFile,
      topN = 3
    )

    candidates should not be empty
    candidates.head.address shouldBe relocatedTo
    candidates.head.variance shouldBe 0.0
  }

  private def simpleModel(tableName: String, zAddress: Long, count: Int): XdfModel = {
    val table = XdfTable(
      uniqueId = 0,
      flags = 0,
      title = tableName,
      description = "",
      categoryMems = Seq.empty,
      axes = Axes(
        x = XdfAxisX(
          id = "x",
          uniqueId = 0,
          embeddedData = EmbeddedData(0, undefinedAddress, 8, 0, count, 0, 0),
          indexCount = count,
          dataType = None,
          unitType = None,
          daLink = None,
          labels = Seq.empty,
          math = None,
          units = ""
        ),
        y = XdfAxisY(
          id = "y",
          uniqueId = 0,
          embeddedData = EmbeddedData(0, undefinedAddress, 8, 0, 1, 0, 0),
          indexCount = 1,
          dataType = None,
          unitType = None,
          daLink = None,
          labels = Seq.empty,
          math = None,
          units = ""
        ),
        z = XdfAxisZ(
          id = "z",
          embeddedData = EmbeddedData(0, zAddress, 8, 1, count, 0, 0),
          decimalPl = None,
          min = None,
          max = None,
          outputType = None,
          math = None,
          units = ""
        )
      )
    )

    XdfModel(
      version = "1.0",
      xdfHeader = XdfHeader(
        flags = 0,
        description = "",
        baseOffset = BaseOffset(0, 0),
        defaults = Defaults(8, 0, 0, 0, 0, 0),
        region = Region(0L, 0L, 0L, 0, 0, "", ""),
        categories = Seq.empty
      ),
      tables = Seq(table),
      virtualTables = Seq.empty
    )
  }
}
