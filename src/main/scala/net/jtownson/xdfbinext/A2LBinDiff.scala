package net.jtownson.xdfbinext

import net.alenzen.a2l.*
import net.jtownson.xdfbinext.A2LBinAdapter.CharacteristicValue
import net.jtownson.xdfbinext.A2LBinDiff.Diff
import net.jtownson.xdfbinext.a2l.CharacteristicSummary

import java.io.File

class A2LBinDiff(binFileLhs: File, binFileRhs: File, a2l: A2LWrapper, offset: Long = 0x9000000) {

  private val binLhs: A2LBinAdapter = A2LBinAdapter(binFileLhs, a2l, offset)
  private val binRhs: A2LBinAdapter = A2LBinAdapter(binFileRhs, a2l, offset)

  val diffs: Iterable[(Characteristic, Diff)] = a2l.characteristics.values
    .map(c => c -> (binLhs.readCharacteristic(c.getName), binRhs.readCharacteristic(c.getName)))
    .collect { case (n, (v1, v2)) if v1 != v2 => n -> (v1, v2) }
    .map { case (c, (lhs, rhs)) => c -> Diff(a2l.getSummary(c.getName), lhs, rhs) }

  lazy val diffsByUsage: List[(Characteristic, Diff)] =
    diffs.toList.sortBy((c, diff) => diff.summary.referencedBy.mkString)

}

object A2LBinDiff {
  case class Diff(
      summary: CharacteristicSummary,
      lhs: CharacteristicValue,
      rhs: CharacteristicValue
  )
}
