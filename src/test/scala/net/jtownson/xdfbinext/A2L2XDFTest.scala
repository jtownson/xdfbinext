package net.jtownson.xdfbinext

import org.scalatest.flatspec.AnyFlatSpec

class A2L2XDFTest extends AnyFlatSpec {

  def withA2L(test: A2L2XDF => Any): Unit = {
    val a2lUrl  = getClass.getResource("/DME861_R1C9J8B3B.a2l").toURI.toURL
    val a2l2Dot = new A2L2XDF(a2lUrl)
    test(a2l2Dot)
  }

  it should "output an XDF snippet for an A2L characteristic" ignore {
    withA2L(a2l => println(a2l.characteristic2XDF(_ == "K_MDKIST_SOT_MX").head))
  }

  it should "output an XDF snippet for an A2L curve" ignore {
    withA2L(a2l => println(a2l.characteristic2XDF(_ == "KL_LAMX").head))
  }

  it should "output an XDF snippet for an A2L map" ignore {
    withA2L(a2l => println(a2l.characteristic2XDF(_ == "BMWtchctr_p_DifCrtnPp_M").head))
  }

  it should "output an XDF snippet for KF_AUSY_TURB" ignore {
    withA2L(a2l => println(a2l.characteristic2XDF(_ == "BMWausy_swi_p_DifCatTot_C").head))
  }

  it should "output XDF snippets for new XDF tables" ignore {
    val newTables =
      Set(
        "BMWausy_p_DifCat_T",
        "KL_FPKATREL_PUMG",
        "KF_AUSY_TURB_DIFF",
        "KL_AUSY_TURB",
        "KF_AUSY_TURB",
        "BMWtchctr_fac_TrbEffIvs_T",
        "BMWtchctr_fac_TrbExp_T",
        "BMWtchctr_t_ExGasMdl_M",
        "BMWtchout_f_GasWg_M"
      )
    withA2L(a2l => println(a2l.characteristic2XDF(newTables.contains).mkString("\n\n")))
  }

  it should "create tables for every characteristic without crashing" in {

  }
}
