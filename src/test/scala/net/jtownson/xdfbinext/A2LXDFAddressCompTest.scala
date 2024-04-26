package net.jtownson.xdfbinext

import org.scalatest.flatspec.AnyFlatSpec

class A2LXDFAddressCompTest extends AnyFlatSpec {

  val a2lUrl  = getClass.getResource("/DME861_R1C9J8B3B.a2l").toURI.toURL
  val a2l2Dot = new A2l2Dot(a2lUrl)

  it should "read an XDF and an a2l and compare addresses of known tables" in {

    val a2lAddresses =
      List(0x968a80c, 0x96b6a0a, 0x96b6c9e, 0x96b6b76, 0x96b6ade)

    val a2lControl = 0x96b6b7e
    val xdfControl = 0x6b6b7e

    val offset = a2lControl - xdfControl
    println(offset.toHexString)
    val xdfAddresses = a2lAddresses.map(a2lAddress => a2lAddress - offset).map(l => s"0x${l.toHexString}")

    println(xdfAddresses.mkString(", "))
  }

  it should "output an XDF snippet for an A2L characteristic" in {
    println(a2l2Dot.characteristic2XDF(_ == "K_MDKIST_SOT_MX").head)
    succeed
  }

  it should "output an XDF snippet for an A2L curve" in {
    println(a2l2Dot.characteristic2XDF(_ == "KL_LAMX").head)
    succeed
  }

  it should "output an XDF snippet for an A2L map" in {
    println(a2l2Dot.characteristic2XDF(_ == "BMWtchctr_p_DifCrtnPp_M").head)
  }

  it should "output an XDF snippet for KF_AUSY_TURB" in {
    println(a2l2Dot.characteristic2XDF(_ == "KF_AUSY_TURB").head)
  }

  it should "output XDF snippets for new XDF tables" in {
    val newTables =
      Set(
        "BMWtchctr_fac_TrbEffIvs_T",
        "BMWtchctr_fac_TrbExp_T",
        "BMWtchctr_cpp_ExGas_T",
        "BMWtchctr_t_ExGasMdl_M",
        "BMWtchout_f_GasWg_M"
      )
    println(a2l2Dot.characteristic2XDF(newTables.contains).mkString("\n"))
  }
}
