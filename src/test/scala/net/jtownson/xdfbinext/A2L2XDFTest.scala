package net.jtownson.xdfbinext

import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import scala.io.Source
import scala.util.Using

class A2L2XDFTest extends AnyFlatSpec {

  private val modifiedXDFFile = new File(
    "C:\\Users\\Jeremy\\Documents\\Car\\tuning\\BMW-XDFs\\B58gen1\\00003076501103.xdf"
  )

  val modXdf = Using.resource(Source.fromFile(modifiedXDFFile))(xdfSource => XdfParser.parse(xdfSource.mkString))
  val a2lUrl = getClass.getResource("/DME861_R1C9J8B3B.a2l").toURI.toURL
  val a2l    = new A2L2XDF(a2lUrl = a2lUrl, xdfModel = modXdf)

  it should "calculate offsets" ignore {
    val vals = Seq(0x77722228, 0x7772221c, 0x77722034)
    val off = 0x24B0

    vals.foreach{i =>
      val no = i + off
      println(f"0x$i%08X -> 0x$no%08X")
    }
  }
  
  it should "write thingys tables" ignore {
    val xdfFile = new File("C:\\Users\\Jeremy\\Documents\\Car\\tuning\\BMW-XDFs\\B58gen2\\00005D55466408\\00005D55466408.xdf")

    val xdf = Using.resource(Source.fromFile(xdfFile))(xdfSource => XdfParser.parse(xdfSource.mkString))
    val a2lUrl = getClass.getResource("/DME86S0_F4C9G576B.a2l").toURI.toURL
    val a2l = new A2L2XDF(a2lUrl = a2lUrl, xdfModel = xdf)

    val tables = List(
      "BMWtchctr_fac_IpPctWg_M"
    )

    a2l.characteristic2XDF(tables).foreach(println)
  }

  it should "compare table addresses for two XDFs" in {
    pending
  }

  it should "write tables for gauntlet HPFP" in {

    val tables = List(
      "BMWchas_p_RailInjSng_M",
      "MfVDTypC_KLVZMSVUB",
      "KFVSTMSVO",
      "KLVSTMSVG",
      "FWIHDR",
      "MfVlv_iHold_C"
    )

    a2l.characteristic2XDF(tables).foreach(println)
  }

  it should "write tables for nostrum injectors" in {
    val tables = List(
      "InjVlvPs_Dset0_iBoost_max_C",
      "InjVlvPs_Dset0_iBoost_min_C",
      "InjVlvPs_Dset0_iDcdcChrg_max_C",
      "InjVlvPs_Dset0_iHold_max_C",
      "InjVlvPs_Dset0_iHold_min_C",
      "InjVlvPs_Dset0_iPull_max_C",
      "InjVlvPs_Dset0_iPull_min_C",
      "InjVlvPs_Dset1_iBoost_max_C",
      "InjVlvPs_Dset1_iBoost_min_C",
      "InjVlvPs_Dset1_iDcdcChrg_max_C",
      "InjVlvPs_Dset1_iHold_max_C",
      "InjVlvPs_Dset1_iHold_min_C",
      "InjVlvPs_Dset1_iPull_max_C",
      "InjVlvPs_Dset1_iPull_min_C",
      "InjVlvPs_Dset1_tiBoost_C",
      "InjVlvPs_Dset1_tiPull_C",
      "InjVlvPs_Dset2_iBoost_max_C",
      "InjVlvPs_Dset2_iBoost_min_C",
      "InjVlvPs_Dset2_iDcdcChrg_max_C",
      "InjVlvPs_Dset2_iHold_max_C",
      "InjVlvPs_Dset2_iHold_min_C",
      "InjVlvPs_Dset2_iPull_max_C",
      "InjVlvPs_Dset2_iPull_min_C",
      "BMWlpsd_p_RailMin_C",
      "MK2TE"
    )

    a2l.characteristic2XDF(tables).foreach(println)
  }

  it should "output K_F_SCHALTEINGRIFF" in {
    val a2lFile = new File(
      "C:\\Users\\Jeremy\\Documents\\Car\\tuning\\MHD BMW Maps\\F-G Series\\s58_____MG1CS024 F4C9G595B+damos\\00005c640a6405_____MG1CS024 F4C9G595B\\F4C9G576B\\Asap2_9\\DME86S0_F4C9G576B.a2l"
    )
    val a2lUrl = a2lFile.toURI.toURL
    val xdfFile = new File(
      "C:\\Users\\Jeremy\\Documents\\Car\\tuning\\BMW-XDFs\\B58gen2\\00005D553CAA08\\00005D553CAA08.xdf"
    )
    val xdfModel = Using.resource(Source.fromFile(xdfFile))(xdfSource => XdfParser.parse(xdfSource.mkString))
    val a2l      = new A2L2XDF(a2lUrl = a2lUrl, xdfModel = xdfModel)

    println(a2l.characteristic2XDF(_ == "K_F_SCHALTEINGRIFF").head)
  }

  it should "output a snippet for exhuast tables" in {
    println(a2l.characteristic2XDF(_ == "KF_TA_ATLSOLL").head)
  }

  it should "output an XDF snippet for an A2L characteristic" in {
    // TODO (VAL_BLK)
    pending
//    println(a2l.characteristic2XDF(_ == "K_I_GANG_AT").head)
  }

  it should "output an XDF snippet for an A2L curve" ignore {
    println(a2l.characteristic2XDF(_ == "KL_LAMX").head)
  }

  it should "output an XDF snippet for an A2L map" ignore {
    println(a2l.characteristic2XDF(_ == "BMWtchctr_p_DifCrtnPp_M").head)
  }

  it should "output an XDF snippet for KF_AUSY_TURB" ignore {
    println(a2l.characteristic2XDF(_ == "BMWtchctr_fac_FilRatWg_T").head)
  }

  it should "output XDF snippets for tchsp tables" in {
    val newTables =
      List(
//        "BMWausy_p_DifCat_T",
//        "KL_AUSY_TURB",
//        "KF_AUSY_TURB",
//        "KL_AUSY_TURB_DIFF",
//        "KL_TABG2TABG_WZINV",
//        "BMWtchctr_fac_TrbEffIvs_T",
//        "BMWtchctr_fac_TrbExp_T"
//        "BMWtchctr_fac_TrbEffPlsCrtn_M",
        "BMWtchctr_cpp_ExGas_T"
        //        "BMWtchctr_p_ReqDynLimPctl_C",
        //        "BMWausy_gra_PEgFlpDwn_C",
        //        "BMWausy_gra_PEgFlpOp_C"
        //        "BMWtchsp_rat_p_CmprPmp_T",
        //        "BMWtchsp_rat_p_CmprMax_M",
        //        "BMWtchsp_p_ReqMax_C",
        //        "BMWtchsp_fac_FilPRatCmpr_T",
        //        "BMWtchsp_fac_mf_FilCmprNorm_T",
        //        "BMWtchctr_pwr_Pctl_M",
        //        "BMWtchctr_pwr_CmprGra_M",
        //        "BMWtchctr_p_DifCrtnPp_M",
        //        "BMWtchctr_pwr_Dp_M",
        //        "BMWtchctr_fac_Dp_T",
        //        "BMWtchco_p_HysAcv_C",
        //        "BMWtchctr_fac_pwr_CompLimPctl_T",
        //        "BMWtchctr_rat_p_ComprLimPctl_T ",
//        "BMWtchctr_pct_WgBasc_M"
      )

    newTables.foreach { table =>
      println(a2l.characteristic2XDF(_ == table).mkString("\n\n"))
      println
    }
  }
}
