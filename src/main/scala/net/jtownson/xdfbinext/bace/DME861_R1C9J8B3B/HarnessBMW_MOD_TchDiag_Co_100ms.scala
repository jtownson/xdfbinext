package net.jtownson.xdfbinext.bace.DME861_R1C9J8B3B

import net.jtownson.xdfbinext.a2l.{CurveType, MapType}
import net.jtownson.xdfbinext.bace.BaceDSL.MeasurementType.OutMeasurement

import net.jtownson.xdfbinext.A2LBinAdapter

object HarnessBMW_MOD_TchDiag_Co_100ms {

  val BMWtchdiag_b_AdpOff_bo  = OutMeasurement[BigDecimal]()
  val BMWtchdiag_b_CtlrOff_bo = OutMeasurement[BigDecimal]()
  val BMWtchdiag_b_DiagOff_bo = OutMeasurement[BigDecimal]()
  val BMWtchdiag_b_LimPctl_bo = OutMeasurement[BigDecimal]()
  val BMWtchdiag_b_rf_Lim_bo  = OutMeasurement[BigDecimal]()
  val BMWtchdiag_b_tq_Lim_bo  = OutMeasurement[BigDecimal]()

  def BMW_MOD_TchDiag_Co_100ms(a2lBin: A2LBinAdapter)(
      BMWeisy_b_PSnsrDblErr_bo: BigDecimal,
      BMWeisy_b_PSnsrPreThrErr_bo: BigDecimal,
      BMWemm_ct_StDrv_ub: BigDecimal,
      BMWeng_b_StrEnd_bo: BigDecimal,
      BMWtchdiag_b_AdpOff_bo: OutMeasurement[BigDecimal],
      BMWtchdiag_b_CtlrOff_bo: OutMeasurement[BigDecimal],
      BMWtchdiag_b_DiagOff_bo: OutMeasurement[BigDecimal],
      BMWtchdiag_b_LimPctl_bo: OutMeasurement[BigDecimal],
      BMWtchdiag_b_rf_Lim_bo: OutMeasurement[BigDecimal],
      BMWtchdiag_b_tq_Lim_bo: OutMeasurement[BigDecimal]
  ): Unit = {

    val BMWtchdiag_swi_FidNew_C: String = a2lBin.readCharacteristicWithCast("BMWtchdiag_swi_FidNew_C")
    val BMWtchdiag_swi_Rst_C: String    = a2lBin.readCharacteristicWithCast("BMWtchdiag_swi_Rst_C")

    Tch_039_004_0.BMW_MOD_TchDiag_Co_100ms(
      BMWeisy_b_PSnsrDblErr_bo,
      BMWeisy_b_PSnsrPreThrErr_bo,
      BMWemm_ct_StDrv_ub,
      BMWeng_b_StrEnd_bo,
      BMWtchdiag_swi_FidNew_C,
      BMWtchdiag_swi_Rst_C,
      BMWtchdiag_b_AdpOff_bo,
      BMWtchdiag_b_CtlrOff_bo,
      BMWtchdiag_b_DiagOff_bo,
      BMWtchdiag_b_LimPctl_bo,
      BMWtchdiag_b_rf_Lim_bo,
      BMWtchdiag_b_tq_Lim_bo
    )
  }
}
