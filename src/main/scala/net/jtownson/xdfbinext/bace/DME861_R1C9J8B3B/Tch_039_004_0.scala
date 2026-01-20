
package net.jtownson.xdfbinext.bace.DME861_R1C9J8B3B
import net.jtownson.xdfbinext.bace.BaceDSL.*
import net.jtownson.xdfbinext.A2LBinAdapter
import net.jtownson.xdfbinext.a2l.A2LMeasurement.{InMeasurement, OutMeasurement}
import net.jtownson.xdfbinext.a2l.{CurveType, MapType, A2LMeasurement}

object Tch_039_004_0 {

def BMW_MOD_TchAd_100ms(BMWtchad_fac_Raw_uw: InMeasurement, BMWtchbas_p_Dif_sw: InMeasurement, BMWtchbas_rat_p_BascBefCmpr_uw: InMeasurement, BMWtchco_b_Acv_bo: InMeasurement, BMWtchctr_b_IpRst2_bo: InMeasurement, BMWtchctr_b_IpRst_bo: InMeasurement, BMWtchctr_b_IpStop2_bo: InMeasurement, BMWtchctr_b_IpStop_bo: InMeasurement, BMWtchctr_fac_FadeAdp_ub: InMeasurement, BMWtchctr_fac_Ip_uw: InMeasurement, BMWtchctr_pct_WgCrtd3_uw: InMeasurement, BMWtchctr_pct_WgIp_sw: InMeasurement, BMWtchdiag_b_AdpOff_bo: InMeasurement, BMWtchsp_p_ReqDyn_sw: InMeasurement, BMWtchsp_rat_p_CmprLim_uw: InMeasurement, BMWtchtbc_fac_EngHot_ub: InMeasurement, BMWtchtbc_t_BefCmprMnfAvg_sw: InMeasurement, Nkw: InMeasurement, Tans: InMeasurement, Tmot: InMeasurement, BMWtchad_fac_EngHotMin_C: BigDecimal, BMWtchad_fac_IpMax_C: BigDecimal, BMWtchad_fac_IpMin_C: BigDecimal, BMWtchad_fac_IpOfs_C: BigDecimal, BMWtchad_fac_Ip_C: BigDecimal, BMWtchad_fac_PBasc_T: CurveType[BigDecimal, BigDecimal], BMWtchad_fac_p_CmprLimMax_C: BigDecimal, BMWtchad_n_EngMax_C: BigDecimal, BMWtchad_n_EngMin_C: BigDecimal, BMWtchad_n_Eng_Ca: Array[BigDecimal], BMWtchad_p_DifAbsMax_C: BigDecimal, BMWtchad_p_ReqDynMax_C: BigDecimal, BMWtchad_p_ReqDynMin_C: BigDecimal, BMWtchad_pct_WgIpAdpToDwn_C: BigDecimal, BMWtchad_pct_WgIpAdpToUp_C: BigDecimal, BMWtchad_swi_AcvMan_C: String, BMWtchad_t_EngMin_C: BigDecimal, BMWtchad_t_MnfMax_C: BigDecimal, BMWtchad_ti_DlyAftIpIni_C: BigDecimal, BMWtchctr_fac_FadeAdpMin_C: BigDecimal, BMWtchad_swi_Rst_C: String, BMWtchctr_swi_IpPwrAcvn_C: String, BMWtchad_fac_AdpAry_sl: OutMeasurement, BMWtchad_fac_AdpAry_uw: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchAd_100ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchad_fac_EngHotMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchad_fac_EngHotMin_C")
val BMWtchad_fac_IpMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchad_fac_IpMax_C")
val BMWtchad_fac_IpMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchad_fac_IpMin_C")
val BMWtchad_fac_IpOfs_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchad_fac_IpOfs_C")
val BMWtchad_fac_Ip_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchad_fac_Ip_C")
val BMWtchad_fac_PBasc_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchad_fac_PBasc_T")
val BMWtchad_fac_p_CmprLimMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchad_fac_p_CmprLimMax_C")
val BMWtchad_n_EngMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchad_n_EngMax_C")
val BMWtchad_n_EngMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchad_n_EngMin_C")
val BMWtchad_n_Eng_Ca: Array[BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchad_n_Eng_Ca")
val BMWtchad_p_DifAbsMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchad_p_DifAbsMax_C")
val BMWtchad_p_ReqDynMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchad_p_ReqDynMax_C")
val BMWtchad_p_ReqDynMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchad_p_ReqDynMin_C")
val BMWtchad_pct_WgIpAdpToDwn_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchad_pct_WgIpAdpToDwn_C")
val BMWtchad_pct_WgIpAdpToUp_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchad_pct_WgIpAdpToUp_C")
val BMWtchad_swi_AcvMan_C: String = a2lBin.readCharacteristicWithCast("BMWtchad_swi_AcvMan_C")
val BMWtchad_t_EngMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchad_t_EngMin_C")
val BMWtchad_t_MnfMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchad_t_MnfMax_C")
val BMWtchad_ti_DlyAftIpIni_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchad_ti_DlyAftIpIni_C")
val BMWtchctr_fac_FadeAdpMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_FadeAdpMin_C")
val BMWtchad_swi_Rst_C: String = a2lBin.readCharacteristicWithCast("BMWtchad_swi_Rst_C")
val BMWtchctr_swi_IpPwrAcvn_C: String = a2lBin.readCharacteristicWithCast("BMWtchctr_swi_IpPwrAcvn_C")
val BMWtchad_fac_Raw_uw: InMeasurement = a2lBin.measurement("BMWtchad_fac_Raw_uw")
val BMWtchbas_p_Dif_sw: InMeasurement = a2lBin.measurement("BMWtchbas_p_Dif_sw")
val BMWtchbas_rat_p_BascBefCmpr_uw: InMeasurement = a2lBin.measurement("BMWtchbas_rat_p_BascBefCmpr_uw")
val BMWtchco_b_Acv_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Acv_bo")
val BMWtchctr_b_IpRst2_bo: InMeasurement = a2lBin.measurement("BMWtchctr_b_IpRst2_bo")
val BMWtchctr_b_IpRst_bo: InMeasurement = a2lBin.measurement("BMWtchctr_b_IpRst_bo")
val BMWtchctr_b_IpStop2_bo: InMeasurement = a2lBin.measurement("BMWtchctr_b_IpStop2_bo")
val BMWtchctr_b_IpStop_bo: InMeasurement = a2lBin.measurement("BMWtchctr_b_IpStop_bo")
val BMWtchctr_fac_FadeAdp_ub: InMeasurement = a2lBin.measurement("BMWtchctr_fac_FadeAdp_ub")
val BMWtchctr_fac_Ip_uw: InMeasurement = a2lBin.measurement("BMWtchctr_fac_Ip_uw")
val BMWtchctr_pct_WgCrtd3_uw: InMeasurement = a2lBin.measurement("BMWtchctr_pct_WgCrtd3_uw")
val BMWtchctr_pct_WgIp_sw: InMeasurement = a2lBin.measurement("BMWtchctr_pct_WgIp_sw")
val BMWtchdiag_b_AdpOff_bo: InMeasurement = a2lBin.measurement("BMWtchdiag_b_AdpOff_bo")
val BMWtchsp_p_ReqDyn_sw: InMeasurement = a2lBin.measurement("BMWtchsp_p_ReqDyn_sw")
val BMWtchsp_rat_p_CmprLim_uw: InMeasurement = a2lBin.measurement("BMWtchsp_rat_p_CmprLim_uw")
val BMWtchtbc_fac_EngHot_ub: InMeasurement = a2lBin.measurement("BMWtchtbc_fac_EngHot_ub")
val BMWtchtbc_t_BefCmprMnfAvg_sw: InMeasurement = a2lBin.measurement("BMWtchtbc_t_BefCmprMnfAvg_sw")
val Nkw: InMeasurement = a2lBin.measurement("Nkw")
val Tans: InMeasurement = a2lBin.measurement("Tans")
val Tmot: InMeasurement = a2lBin.measurement("Tmot")
val BMWtchad_fac_AdpAry_sl: OutMeasurement = a2lBin.measurement("BMWtchad_fac_AdpAry_sl")
val BMWtchad_fac_AdpAry_uw: OutMeasurement = a2lBin.measurement("BMWtchad_fac_AdpAry_uw")

  BMW_MOD_TchAd_100ms(BMWtchad_fac_Raw_uw, BMWtchbas_p_Dif_sw, BMWtchbas_rat_p_BascBefCmpr_uw, BMWtchco_b_Acv_bo, BMWtchctr_b_IpRst2_bo, BMWtchctr_b_IpRst_bo, BMWtchctr_b_IpStop2_bo, BMWtchctr_b_IpStop_bo, BMWtchctr_fac_FadeAdp_ub, BMWtchctr_fac_Ip_uw, BMWtchctr_pct_WgCrtd3_uw, BMWtchctr_pct_WgIp_sw, BMWtchdiag_b_AdpOff_bo, BMWtchsp_p_ReqDyn_sw, BMWtchsp_rat_p_CmprLim_uw, BMWtchtbc_fac_EngHot_ub, BMWtchtbc_t_BefCmprMnfAvg_sw, Nkw, Tans, Tmot, BMWtchad_fac_EngHotMin_C, BMWtchad_fac_IpMax_C, BMWtchad_fac_IpMin_C, BMWtchad_fac_IpOfs_C, BMWtchad_fac_Ip_C, BMWtchad_fac_PBasc_T, BMWtchad_fac_p_CmprLimMax_C, BMWtchad_n_EngMax_C, BMWtchad_n_EngMin_C, BMWtchad_n_Eng_Ca, BMWtchad_p_DifAbsMax_C, BMWtchad_p_ReqDynMax_C, BMWtchad_p_ReqDynMin_C, BMWtchad_pct_WgIpAdpToDwn_C, BMWtchad_pct_WgIpAdpToUp_C, BMWtchad_swi_AcvMan_C, BMWtchad_t_EngMin_C, BMWtchad_t_MnfMax_C, BMWtchad_ti_DlyAftIpIni_C, BMWtchctr_fac_FadeAdpMin_C, BMWtchad_swi_Rst_C, BMWtchctr_swi_IpPwrAcvn_C, BMWtchad_fac_AdpAry_sl, BMWtchad_fac_AdpAry_uw)
}


def BMW_MOD_TchAd_10ms(BMWtchad_fac_AdpAry_uw: InMeasurement, BMWtchco_b_Clc10MilliSec_bo: InMeasurement, BMWtchsp_rat_p_CmprLim_uw: InMeasurement, Nkw: InMeasurement, BMWtchad_fac_AdpCrtn_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchad_n_Eng_Ca: Array[BigDecimal], BMWtchad_fac_Raw_uw: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchAd_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchad_fac_AdpCrtn_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchad_fac_AdpCrtn_M")
val BMWtchad_n_Eng_Ca: Array[BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchad_n_Eng_Ca")
val BMWtchad_fac_AdpAry_uw: InMeasurement = a2lBin.measurement("BMWtchad_fac_AdpAry_uw")
val BMWtchco_b_Clc10MilliSec_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Clc10MilliSec_bo")
val BMWtchsp_rat_p_CmprLim_uw: InMeasurement = a2lBin.measurement("BMWtchsp_rat_p_CmprLim_uw")
val Nkw: InMeasurement = a2lBin.measurement("Nkw")
val BMWtchad_fac_Raw_uw: OutMeasurement = a2lBin.measurement("BMWtchad_fac_Raw_uw")

  BMW_MOD_TchAd_10ms(BMWtchad_fac_AdpAry_uw, BMWtchco_b_Clc10MilliSec_bo, BMWtchsp_rat_p_CmprLim_uw, Nkw, BMWtchad_fac_AdpCrtn_M, BMWtchad_n_Eng_Ca, BMWtchad_fac_Raw_uw)
}


def BMW_MOD_TchAd_swini(BMWeng_b_EcuPwsFaild_bo: InMeasurement, BMWtchad_fac_AdpAry_sl: InMeasurement, BMWtchad_fac_AdpAry_uw: InMeasurement, BMWtchad_swi_Rst_C: String): Unit = {
 ???
}

def BMW_MOD_TchAd_swini(a2lBin: A2LBinAdapter): Unit = {

val BMWtchad_swi_Rst_C: String = a2lBin.readCharacteristicWithCast("BMWtchad_swi_Rst_C")
val BMWeng_b_EcuPwsFaild_bo: InMeasurement = a2lBin.measurement("BMWeng_b_EcuPwsFaild_bo")
val BMWtchad_fac_AdpAry_sl: InMeasurement = a2lBin.measurement("BMWtchad_fac_AdpAry_sl")
val BMWtchad_fac_AdpAry_uw: InMeasurement = a2lBin.measurement("BMWtchad_fac_AdpAry_uw")


  BMW_MOD_TchAd_swini(BMWeng_b_EcuPwsFaild_bo, BMWtchad_fac_AdpAry_sl, BMWtchad_fac_AdpAry_uw, BMWtchad_swi_Rst_C)
}


def BMW_MOD_TchBas_Misc_10ms(BMWbdy_b_CluOp10_bo: InMeasurement, BMWbdy_b_EgFlp_bo: InMeasurement, BMWeng_b_StrEnd_bo: InMeasurement, BMWtchbas_rat_p_BefThrAmb_uw: InMeasurement, BMWtchco_b_Clc10MilliSec_bo: InMeasurement, BMWtchco_b_Req_bo: InMeasurement, BMWtqe_b_DfcoFas_bo: InMeasurement, BMWtqe_b_Dfco_bo: InMeasurement, BMWtqe_b_Noise_bo: InMeasurement, BMWveh_b_TranDrp_bo: InMeasurement, Msakzu: InMeasurement, Nkw: InMeasurement, Nstat: InMeasurement, Pwg_ist: InMeasurement, St_ba_agf: InMeasurement, St_ba_agf_B_kath_korr: InMeasurement, St_bls: InMeasurement, St_bls_B_bls: InMeasurement, St_ngang0: InMeasurement, St_ngang0_B_gangnull: InMeasurement, St_ngang0_B_gangnullerw: InMeasurement, St_ngang0_B_ngangdok: InMeasurement, St_ngang0_B_nggelernt: InMeasurement, St_ngang0_B_ngimlf: InMeasurement, St_ngang0_B_nglernakt: InMeasurement, St_progression: InMeasurement, St_progression_B_eco: InMeasurement, St_progression_B_sport: InMeasurement, V: InMeasurement, V_rad: InMeasurement, Var_at: InMeasurement, Var_dkg: InMeasurement, Var_hs: InMeasurement, BMWtchbas_cw_Noise_C: BigDecimal, BMWtchbas_fac_n_FilDyn_C: BigDecimal, BMWtchbas_mf_NoiseMin_C: BigDecimal, BMWtchbas_n_FlGcMin_C: BigDecimal, BMWtchbas_n_NoiseMin_C: BigDecimal, BMWtchbas_n_OfsNoiseOff_C: BigDecimal, BMWtchbas_nr_n_AvgDyn2_C: BigDecimal, BMWtchbas_pct_AccrFlGcMin_C: BigDecimal, BMWtchbas_pct_AccrFlGcMin_T: CurveType[BigDecimal, BigDecimal], BMWtchbas_pct_AccrGraFlGcMax_C: BigDecimal, BMWtchbas_pct_AccrGradNoiseMin_C: BigDecimal, BMWtchbas_pct_AccrNoiseMin_C: BigDecimal, BMWtchbas_rat_p_BefThrFlGc_C: BigDecimal, BMWtchbas_ti_DlyAccr0FlGcDi_C: BigDecimal, BMWtchbas_ti_DlyFlGcCluDi_C: BigDecimal, BMWtchbas_ti_DlyFlGcEna_C: BigDecimal, BMWtchbas_ti_DlyFlGcHld_C: BigDecimal, BMWtchbas_ti_DlyFlGcLock_C: BigDecimal, BMWtchbas_ti_DlyFlGcMax_C: BigDecimal, BMWtchbas_ti_DlyFlGcPucDi_C: BigDecimal, BMWtchbas_ti_DlyNoiseOff_C: BigDecimal, BMWtchbas_b_FlGcHld_bo: OutMeasurement, BMWtchbas_b_FlGc_bo: OutMeasurement, BMWtchbas_b_Noise_bo: OutMeasurement, BMWtchbas_n_Dyn_sw: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchBas_Misc_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchbas_cw_Noise_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_cw_Noise_C")
val BMWtchbas_fac_n_FilDyn_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_fac_n_FilDyn_C")
val BMWtchbas_mf_NoiseMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_mf_NoiseMin_C")
val BMWtchbas_n_FlGcMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_n_FlGcMin_C")
val BMWtchbas_n_NoiseMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_n_NoiseMin_C")
val BMWtchbas_n_OfsNoiseOff_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_n_OfsNoiseOff_C")
val BMWtchbas_nr_n_AvgDyn2_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_nr_n_AvgDyn2_C")
val BMWtchbas_pct_AccrFlGcMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_pct_AccrFlGcMin_C")
val BMWtchbas_pct_AccrFlGcMin_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchbas_pct_AccrFlGcMin_T")
val BMWtchbas_pct_AccrGraFlGcMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_pct_AccrGraFlGcMax_C")
val BMWtchbas_pct_AccrGradNoiseMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_pct_AccrGradNoiseMin_C")
val BMWtchbas_pct_AccrNoiseMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_pct_AccrNoiseMin_C")
val BMWtchbas_rat_p_BefThrFlGc_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_rat_p_BefThrFlGc_C")
val BMWtchbas_ti_DlyAccr0FlGcDi_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_ti_DlyAccr0FlGcDi_C")
val BMWtchbas_ti_DlyFlGcCluDi_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_ti_DlyFlGcCluDi_C")
val BMWtchbas_ti_DlyFlGcEna_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_ti_DlyFlGcEna_C")
val BMWtchbas_ti_DlyFlGcHld_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_ti_DlyFlGcHld_C")
val BMWtchbas_ti_DlyFlGcLock_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_ti_DlyFlGcLock_C")
val BMWtchbas_ti_DlyFlGcMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_ti_DlyFlGcMax_C")
val BMWtchbas_ti_DlyFlGcPucDi_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_ti_DlyFlGcPucDi_C")
val BMWtchbas_ti_DlyNoiseOff_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_ti_DlyNoiseOff_C")
val BMWbdy_b_CluOp10_bo: InMeasurement = a2lBin.measurement("BMWbdy_b_CluOp10_bo")
val BMWbdy_b_EgFlp_bo: InMeasurement = a2lBin.measurement("BMWbdy_b_EgFlp_bo")
val BMWeng_b_StrEnd_bo: InMeasurement = a2lBin.measurement("BMWeng_b_StrEnd_bo")
val BMWtchbas_rat_p_BefThrAmb_uw: InMeasurement = a2lBin.measurement("BMWtchbas_rat_p_BefThrAmb_uw")
val BMWtchco_b_Clc10MilliSec_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Clc10MilliSec_bo")
val BMWtchco_b_Req_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Req_bo")
val BMWtqe_b_DfcoFas_bo: InMeasurement = a2lBin.measurement("BMWtqe_b_DfcoFas_bo")
val BMWtqe_b_Dfco_bo: InMeasurement = a2lBin.measurement("BMWtqe_b_Dfco_bo")
val BMWtqe_b_Noise_bo: InMeasurement = a2lBin.measurement("BMWtqe_b_Noise_bo")
val BMWveh_b_TranDrp_bo: InMeasurement = a2lBin.measurement("BMWveh_b_TranDrp_bo")
val Msakzu: InMeasurement = a2lBin.measurement("Msakzu")
val Nkw: InMeasurement = a2lBin.measurement("Nkw")
val Nstat: InMeasurement = a2lBin.measurement("Nstat")
val Pwg_ist: InMeasurement = a2lBin.measurement("Pwg_ist")
val St_ba_agf: InMeasurement = a2lBin.measurement("St_ba_agf")
val St_ba_agf_B_kath_korr: InMeasurement = a2lBin.measurement("St_ba_agf.B_kath_korr")
val St_bls: InMeasurement = a2lBin.measurement("St_bls")
val St_bls_B_bls: InMeasurement = a2lBin.measurement("St_bls.B_bls")
val St_ngang0: InMeasurement = a2lBin.measurement("St_ngang0")
val St_ngang0_B_gangnull: InMeasurement = a2lBin.measurement("St_ngang0.B_gangnull")
val St_ngang0_B_gangnullerw: InMeasurement = a2lBin.measurement("St_ngang0.B_gangnullerw")
val St_ngang0_B_ngangdok: InMeasurement = a2lBin.measurement("St_ngang0.B_ngangdok")
val St_ngang0_B_nggelernt: InMeasurement = a2lBin.measurement("St_ngang0.B_nggelernt")
val St_ngang0_B_ngimlf: InMeasurement = a2lBin.measurement("St_ngang0.B_ngimlf")
val St_ngang0_B_nglernakt: InMeasurement = a2lBin.measurement("St_ngang0.B_nglernakt")
val St_progression: InMeasurement = a2lBin.measurement("St_progression")
val St_progression_B_eco: InMeasurement = a2lBin.measurement("St_progression.B_eco")
val St_progression_B_sport: InMeasurement = a2lBin.measurement("St_progression.B_sport")
val V: InMeasurement = a2lBin.measurement("V")
val V_rad: InMeasurement = a2lBin.measurement("V_rad")
val Var_at: InMeasurement = a2lBin.measurement("Var_at")
val Var_dkg: InMeasurement = a2lBin.measurement("Var_dkg")
val Var_hs: InMeasurement = a2lBin.measurement("Var_hs")
val BMWtchbas_b_FlGcHld_bo: OutMeasurement = a2lBin.measurement("BMWtchbas_b_FlGcHld_bo")
val BMWtchbas_b_FlGc_bo: OutMeasurement = a2lBin.measurement("BMWtchbas_b_FlGc_bo")
val BMWtchbas_b_Noise_bo: OutMeasurement = a2lBin.measurement("BMWtchbas_b_Noise_bo")
val BMWtchbas_n_Dyn_sw: OutMeasurement = a2lBin.measurement("BMWtchbas_n_Dyn_sw")

  BMW_MOD_TchBas_Misc_10ms(BMWbdy_b_CluOp10_bo, BMWbdy_b_EgFlp_bo, BMWeng_b_StrEnd_bo, BMWtchbas_rat_p_BefThrAmb_uw, BMWtchco_b_Clc10MilliSec_bo, BMWtchco_b_Req_bo, BMWtqe_b_DfcoFas_bo, BMWtqe_b_Dfco_bo, BMWtqe_b_Noise_bo, BMWveh_b_TranDrp_bo, Msakzu, Nkw, Nstat, Pwg_ist, St_ba_agf, St_ba_agf_B_kath_korr, St_bls, St_bls_B_bls, St_ngang0, St_ngang0_B_gangnull, St_ngang0_B_gangnullerw, St_ngang0_B_ngangdok, St_ngang0_B_nggelernt, St_ngang0_B_ngimlf, St_ngang0_B_nglernakt, St_progression, St_progression_B_eco, St_progression_B_sport, V, V_rad, Var_at, Var_dkg, Var_hs, BMWtchbas_cw_Noise_C, BMWtchbas_fac_n_FilDyn_C, BMWtchbas_mf_NoiseMin_C, BMWtchbas_n_FlGcMin_C, BMWtchbas_n_NoiseMin_C, BMWtchbas_n_OfsNoiseOff_C, BMWtchbas_nr_n_AvgDyn2_C, BMWtchbas_pct_AccrFlGcMin_C, BMWtchbas_pct_AccrFlGcMin_T, BMWtchbas_pct_AccrGraFlGcMax_C, BMWtchbas_pct_AccrGradNoiseMin_C, BMWtchbas_pct_AccrNoiseMin_C, BMWtchbas_rat_p_BefThrFlGc_C, BMWtchbas_ti_DlyAccr0FlGcDi_C, BMWtchbas_ti_DlyFlGcCluDi_C, BMWtchbas_ti_DlyFlGcEna_C, BMWtchbas_ti_DlyFlGcHld_C, BMWtchbas_ti_DlyFlGcLock_C, BMWtchbas_ti_DlyFlGcMax_C, BMWtchbas_ti_DlyFlGcPucDi_C, BMWtchbas_ti_DlyNoiseOff_C, BMWtchbas_b_FlGcHld_bo, BMWtchbas_b_FlGc_bo, BMWtchbas_b_Noise_bo, BMWtchbas_n_Dyn_sw)
}


def BMW_MOD_TchBas_P_10ms(BMWeisy_p_PreThrPlau_uw: InMeasurement, BMWtchco_b_Clc10MilliSec_bo: InMeasurement, BMWtchco_st_Opm_ub: InMeasurement, BMWtchsp_p_Req_uw: InMeasurement, BMWtchsp_volf_BefTc_uw: InMeasurement, BMWtchtbc_t_BefCmpr_sw: InMeasurement, Nkw: InMeasurement, Pumg: InMeasurement, BMWtchbas_fac_FilPBefThrDyn_C: BigDecimal, BMWtchbas_fac_FilPDifCrtn_T: CurveType[BigDecimal, BigDecimal], BMWtchbas_fac_FilPDif_T: CurveType[BigDecimal, BigDecimal], BMWtchbas_fac_TcBascCrtn_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchbas_nr_PDifGraAvg_C: BigDecimal, BMWtchbas_p_DifAmbBefCmpr_T: CurveType[BigDecimal, BigDecimal], BMWtchbas_rat_p_TcBascCond_T: CurveType[BigDecimal, BigDecimal], BMWtchbas_rat_p_TcBascEco_T: CurveType[BigDecimal, BigDecimal], BMWtchbas_rat_p_TcBascRoofOp_T: CurveType[BigDecimal, BigDecimal], BMWtchbas_rat_p_TcBascSpt_T: CurveType[BigDecimal, BigDecimal], BMWtchbas_rat_p_TcBasc_T: CurveType[BigDecimal, BigDecimal], BMWtchbas_p_BefCmpr_uw: OutMeasurement, BMWtchbas_p_BefThrDyn_sw: OutMeasurement, BMWtchbas_p_DifGraRaw_sw: OutMeasurement, BMWtchbas_p_DifGra_sw: OutMeasurement, BMWtchbas_p_Dif_sw: OutMeasurement, BMWtchbas_p_TcBasc_uw: OutMeasurement, BMWtchbas_rat_p_BascBefCmpr_uw: OutMeasurement, BMWtchbas_rat_p_BefCmpr_uw: OutMeasurement, BMWtchbas_rat_p_BefThrAmb_uw: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchBas_P_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchbas_fac_FilPBefThrDyn_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_fac_FilPBefThrDyn_C")
val BMWtchbas_fac_FilPDifCrtn_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchbas_fac_FilPDifCrtn_T")
val BMWtchbas_fac_FilPDif_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchbas_fac_FilPDif_T")
val BMWtchbas_fac_TcBascCrtn_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchbas_fac_TcBascCrtn_M")
val BMWtchbas_nr_PDifGraAvg_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_nr_PDifGraAvg_C")
val BMWtchbas_p_DifAmbBefCmpr_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchbas_p_DifAmbBefCmpr_T")
val BMWtchbas_rat_p_TcBascCond_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchbas_rat_p_TcBascCond_T")
val BMWtchbas_rat_p_TcBascEco_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchbas_rat_p_TcBascEco_T")
val BMWtchbas_rat_p_TcBascRoofOp_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchbas_rat_p_TcBascRoofOp_T")
val BMWtchbas_rat_p_TcBascSpt_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchbas_rat_p_TcBascSpt_T")
val BMWtchbas_rat_p_TcBasc_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchbas_rat_p_TcBasc_T")
val BMWeisy_p_PreThrPlau_uw: InMeasurement = a2lBin.measurement("BMWeisy_p_PreThrPlau_uw")
val BMWtchco_b_Clc10MilliSec_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Clc10MilliSec_bo")
val BMWtchco_st_Opm_ub: InMeasurement = a2lBin.measurement("BMWtchco_st_Opm_ub")
val BMWtchsp_p_Req_uw: InMeasurement = a2lBin.measurement("BMWtchsp_p_Req_uw")
val BMWtchsp_volf_BefTc_uw: InMeasurement = a2lBin.measurement("BMWtchsp_volf_BefTc_uw")
val BMWtchtbc_t_BefCmpr_sw: InMeasurement = a2lBin.measurement("BMWtchtbc_t_BefCmpr_sw")
val Nkw: InMeasurement = a2lBin.measurement("Nkw")
val Pumg: InMeasurement = a2lBin.measurement("Pumg")
val BMWtchbas_p_BefCmpr_uw: OutMeasurement = a2lBin.measurement("BMWtchbas_p_BefCmpr_uw")
val BMWtchbas_p_BefThrDyn_sw: OutMeasurement = a2lBin.measurement("BMWtchbas_p_BefThrDyn_sw")
val BMWtchbas_p_DifGraRaw_sw: OutMeasurement = a2lBin.measurement("BMWtchbas_p_DifGraRaw_sw")
val BMWtchbas_p_DifGra_sw: OutMeasurement = a2lBin.measurement("BMWtchbas_p_DifGra_sw")
val BMWtchbas_p_Dif_sw: OutMeasurement = a2lBin.measurement("BMWtchbas_p_Dif_sw")
val BMWtchbas_p_TcBasc_uw: OutMeasurement = a2lBin.measurement("BMWtchbas_p_TcBasc_uw")
val BMWtchbas_rat_p_BascBefCmpr_uw: OutMeasurement = a2lBin.measurement("BMWtchbas_rat_p_BascBefCmpr_uw")
val BMWtchbas_rat_p_BefCmpr_uw: OutMeasurement = a2lBin.measurement("BMWtchbas_rat_p_BefCmpr_uw")
val BMWtchbas_rat_p_BefThrAmb_uw: OutMeasurement = a2lBin.measurement("BMWtchbas_rat_p_BefThrAmb_uw")

  BMW_MOD_TchBas_P_10ms(BMWeisy_p_PreThrPlau_uw, BMWtchco_b_Clc10MilliSec_bo, BMWtchco_st_Opm_ub, BMWtchsp_p_Req_uw, BMWtchsp_volf_BefTc_uw, BMWtchtbc_t_BefCmpr_sw, Nkw, Pumg, BMWtchbas_fac_FilPBefThrDyn_C, BMWtchbas_fac_FilPDifCrtn_T, BMWtchbas_fac_FilPDif_T, BMWtchbas_fac_TcBascCrtn_M, BMWtchbas_nr_PDifGraAvg_C, BMWtchbas_p_DifAmbBefCmpr_T, BMWtchbas_rat_p_TcBascCond_T, BMWtchbas_rat_p_TcBascEco_T, BMWtchbas_rat_p_TcBascRoofOp_T, BMWtchbas_rat_p_TcBascSpt_T, BMWtchbas_rat_p_TcBasc_T, BMWtchbas_p_BefCmpr_uw, BMWtchbas_p_BefThrDyn_sw, BMWtchbas_p_DifGraRaw_sw, BMWtchbas_p_DifGra_sw, BMWtchbas_p_Dif_sw, BMWtchbas_p_TcBasc_uw, BMWtchbas_rat_p_BascBefCmpr_uw, BMWtchbas_rat_p_BefCmpr_uw, BMWtchbas_rat_p_BefThrAmb_uw)
}


def BMW_MOD_TchBas_Race_10ms(BMWosc_acvn_ModEng_ub: InMeasurement, BMWtchco_b_Clc10MilliSec_bo: InMeasurement, Pwg_ist: InMeasurement, St_bls: InMeasurement, St_bls_B_bls: InMeasurement, St_mdanfahr_cc: InMeasurement, St_mdanfahr_cc_B_anfahr_nmax_disp_flag: InMeasurement, St_mdanfahr_cc_B_rennstart_aktiv: InMeasurement, St_rsta_mdkg: InMeasurement, Var_at: InMeasurement, Var_dkg: InMeasurement, BMWtchbas_acvn_ModEngRaceStrt_C: BigDecimal, BMWtchbas_cw_RaceStr_C: BigDecimal, BMWtchbas_pct_AccrRaceStrtDly_C: BigDecimal, BMWtchbas_pct_AccrRaceStrt_C: BigDecimal, BMWtchbas_swi_RaceVarSel_C: String, BMWtchbas_ti_DlyRaceStr_C: BigDecimal, BMWtchbas_b_RaceStr_bo: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchBas_Race_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchbas_acvn_ModEngRaceStrt_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_acvn_ModEngRaceStrt_C")
val BMWtchbas_cw_RaceStr_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_cw_RaceStr_C")
val BMWtchbas_pct_AccrRaceStrtDly_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_pct_AccrRaceStrtDly_C")
val BMWtchbas_pct_AccrRaceStrt_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_pct_AccrRaceStrt_C")
val BMWtchbas_swi_RaceVarSel_C: String = a2lBin.readCharacteristicWithCast("BMWtchbas_swi_RaceVarSel_C")
val BMWtchbas_ti_DlyRaceStr_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbas_ti_DlyRaceStr_C")
val BMWosc_acvn_ModEng_ub: InMeasurement = a2lBin.measurement("BMWosc_acvn_ModEng_ub")
val BMWtchco_b_Clc10MilliSec_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Clc10MilliSec_bo")
val Pwg_ist: InMeasurement = a2lBin.measurement("Pwg_ist")
val St_bls: InMeasurement = a2lBin.measurement("St_bls")
val St_bls_B_bls: InMeasurement = a2lBin.measurement("St_bls.B_bls")
val St_mdanfahr_cc: InMeasurement = a2lBin.measurement("St_mdanfahr_cc")
val St_mdanfahr_cc_B_anfahr_nmax_disp_flag: InMeasurement = a2lBin.measurement("St_mdanfahr_cc.B_anfahr_nmax_disp_flag")
val St_mdanfahr_cc_B_rennstart_aktiv: InMeasurement = a2lBin.measurement("St_mdanfahr_cc.B_rennstart_aktiv")
val St_rsta_mdkg: InMeasurement = a2lBin.measurement("St_rsta_mdkg")
val Var_at: InMeasurement = a2lBin.measurement("Var_at")
val Var_dkg: InMeasurement = a2lBin.measurement("Var_dkg")
val BMWtchbas_b_RaceStr_bo: OutMeasurement = a2lBin.measurement("BMWtchbas_b_RaceStr_bo")

  BMW_MOD_TchBas_Race_10ms(BMWosc_acvn_ModEng_ub, BMWtchco_b_Clc10MilliSec_bo, Pwg_ist, St_bls, St_bls_B_bls, St_mdanfahr_cc, St_mdanfahr_cc_B_anfahr_nmax_disp_flag, St_mdanfahr_cc_B_rennstart_aktiv, St_rsta_mdkg, Var_at, Var_dkg, BMWtchbas_acvn_ModEngRaceStrt_C, BMWtchbas_cw_RaceStr_C, BMWtchbas_pct_AccrRaceStrtDly_C, BMWtchbas_pct_AccrRaceStrt_C, BMWtchbas_swi_RaceVarSel_C, BMWtchbas_ti_DlyRaceStr_C, BMWtchbas_b_RaceStr_bo)
}


def BMW_MOD_TchBov_Acv_10ms(BMWeng_b_StrEnd_bo: InMeasurement, BMWtchbas_b_FlGcHld_bo: InMeasurement, BMWtchbas_p_DifGraRaw_sw: InMeasurement, BMWtchbas_p_Dif_sw: InMeasurement, BMWtchco_b_Acv_bo: InMeasurement, BMWtchco_b_Clc10MilliSec2_bo: InMeasurement, BMWtchsp_b_RfLimAcv_bo: InMeasurement, Drf_soll: InMeasurement, F_psspu: InMeasurement, Nkw: InMeasurement, Pbremsu: InMeasurement, St_bgkuppl1: InMeasurement, St_bgkuppl1_B_ll: InMeasurement, St_dsc_can: InMeasurement, Status_usecase_antr: InMeasurement, V_can: InMeasurement, BMWtchbov_b_AcvnMan_C: String, BMWtchbov_cw_1_C: BigDecimal, BMWtchbov_cw_DscSpt_C: BigDecimal, BMWtchbov_n_AcvnMax_C: BigDecimal, BMWtchbov_n_DiSpt_T: CurveType[BigDecimal, BigDecimal], BMWtchbov_n_Di_T: CurveType[BigDecimal, BigDecimal], BMWtchbov_n_OfsAcvMin_C: BigDecimal, BMWtchbov_nr_PDifPrdn_C: BigDecimal, BMWtchbov_p_BrkMaxDi_C: BigDecimal, BMWtchbov_p_DifAcvnSpt_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchbov_p_DifAcvn_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchbov_p_DifDeac_C: BigDecimal, BMWtchbov_rat_FiGraAcvn_T: CurveType[BigDecimal, BigDecimal], BMWtchbov_rat_FiGraDeac_C: BigDecimal, BMWtchbov_rat_p_InAmbSpt_C: BigDecimal, BMWtchbov_rat_p_InAmb_C: BigDecimal, BMWtchbov_st_Map_Ca: Array[String], BMWtchbov_swi_Man_C: String, BMWtchbov_ti_AcvnMax_C: BigDecimal, BMWtchbov_ti_AftStrtEndEna_C: BigDecimal, BMWtchbov_ti_DlyFiGraAcvn_C: BigDecimal, BMWtchbov_ti_Lock_C: BigDecimal, BMWtchbov_v_VehMaxDi_C: BigDecimal, BMWtchbov_b_Acv_bo: OutMeasurement, BMWtchbov_st_Opm_ub: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchBov_Acv_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchbov_b_AcvnMan_C: String = a2lBin.readCharacteristicWithCast("BMWtchbov_b_AcvnMan_C")
val BMWtchbov_cw_1_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbov_cw_1_C")
val BMWtchbov_cw_DscSpt_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbov_cw_DscSpt_C")
val BMWtchbov_n_AcvnMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbov_n_AcvnMax_C")
val BMWtchbov_n_DiSpt_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchbov_n_DiSpt_T")
val BMWtchbov_n_Di_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchbov_n_Di_T")
val BMWtchbov_n_OfsAcvMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbov_n_OfsAcvMin_C")
val BMWtchbov_nr_PDifPrdn_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbov_nr_PDifPrdn_C")
val BMWtchbov_p_BrkMaxDi_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbov_p_BrkMaxDi_C")
val BMWtchbov_p_DifAcvnSpt_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchbov_p_DifAcvnSpt_M")
val BMWtchbov_p_DifAcvn_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchbov_p_DifAcvn_M")
val BMWtchbov_p_DifDeac_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbov_p_DifDeac_C")
val BMWtchbov_rat_FiGraAcvn_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchbov_rat_FiGraAcvn_T")
val BMWtchbov_rat_FiGraDeac_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbov_rat_FiGraDeac_C")
val BMWtchbov_rat_p_InAmbSpt_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbov_rat_p_InAmbSpt_C")
val BMWtchbov_rat_p_InAmb_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbov_rat_p_InAmb_C")
val BMWtchbov_st_Map_Ca: Array[String] = a2lBin.readCharacteristicWithCast("BMWtchbov_st_Map_Ca")
val BMWtchbov_swi_Man_C: String = a2lBin.readCharacteristicWithCast("BMWtchbov_swi_Man_C")
val BMWtchbov_ti_AcvnMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbov_ti_AcvnMax_C")
val BMWtchbov_ti_AftStrtEndEna_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbov_ti_AftStrtEndEna_C")
val BMWtchbov_ti_DlyFiGraAcvn_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbov_ti_DlyFiGraAcvn_C")
val BMWtchbov_ti_Lock_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbov_ti_Lock_C")
val BMWtchbov_v_VehMaxDi_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbov_v_VehMaxDi_C")
val BMWeng_b_StrEnd_bo: InMeasurement = a2lBin.measurement("BMWeng_b_StrEnd_bo")
val BMWtchbas_b_FlGcHld_bo: InMeasurement = a2lBin.measurement("BMWtchbas_b_FlGcHld_bo")
val BMWtchbas_p_DifGraRaw_sw: InMeasurement = a2lBin.measurement("BMWtchbas_p_DifGraRaw_sw")
val BMWtchbas_p_Dif_sw: InMeasurement = a2lBin.measurement("BMWtchbas_p_Dif_sw")
val BMWtchco_b_Acv_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Acv_bo")
val BMWtchco_b_Clc10MilliSec2_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Clc10MilliSec2_bo")
val BMWtchsp_b_RfLimAcv_bo: InMeasurement = a2lBin.measurement("BMWtchsp_b_RfLimAcv_bo")
val Drf_soll: InMeasurement = a2lBin.measurement("Drf_soll")
val F_psspu: InMeasurement = a2lBin.measurement("F_psspu")
val Nkw: InMeasurement = a2lBin.measurement("Nkw")
val Pbremsu: InMeasurement = a2lBin.measurement("Pbremsu")
val St_bgkuppl1: InMeasurement = a2lBin.measurement("St_bgkuppl1")
val St_bgkuppl1_B_ll: InMeasurement = a2lBin.measurement("St_bgkuppl1.B_ll")
val St_dsc_can: InMeasurement = a2lBin.measurement("St_dsc_can")
val Status_usecase_antr: InMeasurement = a2lBin.measurement("Status_usecase_antr")
val V_can: InMeasurement = a2lBin.measurement("V_can")
val BMWtchbov_b_Acv_bo: OutMeasurement = a2lBin.measurement("BMWtchbov_b_Acv_bo")
val BMWtchbov_st_Opm_ub: OutMeasurement = a2lBin.measurement("BMWtchbov_st_Opm_ub")

  BMW_MOD_TchBov_Acv_10ms(BMWeng_b_StrEnd_bo, BMWtchbas_b_FlGcHld_bo, BMWtchbas_p_DifGraRaw_sw, BMWtchbas_p_Dif_sw, BMWtchco_b_Acv_bo, BMWtchco_b_Clc10MilliSec2_bo, BMWtchsp_b_RfLimAcv_bo, Drf_soll, F_psspu, Nkw, Pbremsu, St_bgkuppl1, St_bgkuppl1_B_ll, St_dsc_can, Status_usecase_antr, V_can, BMWtchbov_b_AcvnMan_C, BMWtchbov_cw_1_C, BMWtchbov_cw_DscSpt_C, BMWtchbov_n_AcvnMax_C, BMWtchbov_n_DiSpt_T, BMWtchbov_n_Di_T, BMWtchbov_n_OfsAcvMin_C, BMWtchbov_nr_PDifPrdn_C, BMWtchbov_p_BrkMaxDi_C, BMWtchbov_p_DifAcvnSpt_M, BMWtchbov_p_DifAcvn_M, BMWtchbov_p_DifDeac_C, BMWtchbov_rat_FiGraAcvn_T, BMWtchbov_rat_FiGraDeac_C, BMWtchbov_rat_p_InAmbSpt_C, BMWtchbov_rat_p_InAmb_C, BMWtchbov_st_Map_Ca, BMWtchbov_swi_Man_C, BMWtchbov_ti_AcvnMax_C, BMWtchbov_ti_AftStrtEndEna_C, BMWtchbov_ti_DlyFiGraAcvn_C, BMWtchbov_ti_Lock_C, BMWtchbov_v_VehMaxDi_C, BMWtchbov_b_Acv_bo, BMWtchbov_st_Opm_ub)
}


def BMW_MOD_TchBov_Mf_10ms(BMWeisy_p_PreThrPlau_uw: InMeasurement, BMWosc_fac_DfcoDrvgSit_ub: InMeasurement, BMWtchbas_p_BefCmpr_uw: InMeasurement, BMWtchbov_b_Acv_bo: InMeasurement, BMWtchbov_st_Opm_ub: InMeasurement, BMWtchco_b_Clc10MilliSec_bo: InMeasurement, BMWtchsp_p_DifIco_uw: InMeasurement, BMWtqe_b_IgTqAcv_bo: InMeasurement, Nkw: InMeasurement, St_dps: InMeasurement, St_dps_B_gd: InMeasurement, V_can: InMeasurement, BMWtchbov_cw_CylOffEna_C: BigDecimal, BMWtchbov_fac_FadeDec_T: CurveType[BigDecimal, BigDecimal], BMWtchbov_fac_mf_ThrEcoMin_C: BigDecimal, BMWtchbov_fac_mf_ThrMin_C: BigDecimal, BMWtchbov_fac_mf_ThrSptMin_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchbov_fac_mf_ThrThrEcoMin_C: BigDecimal, BMWtchbov_fac_mf_ThrThrMin_C: BigDecimal, BMWtchbov_fac_mf_ThrThrSptMin_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchbov_fac_mf_VvtEcoMin_C: BigDecimal, BMWtchbov_fac_mf_VvtMin_C: BigDecimal, BMWtchbov_fac_mf_VvtSptMin_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchbov_mf_PmpLim_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchbov_rat_p_CmprThdVlvOp_C: BigDecimal, BMWtchbov_swi_CylOff0Flow_C: String, BMWtchbov_swi_TiRmp_C: String, BMWtchbov_swi_VlvOpAll_C: String, BMWtchbov_ti_HldFlow_T: CurveType[BigDecimal, BigDecimal], BMWtchbov_ti_ToutIgTqAcv_C: BigDecimal, BMWtchbov_b_CylOffEna_bo: OutMeasurement, BMWtchbov_b_VlvOp_bo: OutMeasurement, BMWtchbov_mf_ThrMin_uw: OutMeasurement, BMWtchbov_mf_VvtMin_uw: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchBov_Mf_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchbov_cw_CylOffEna_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbov_cw_CylOffEna_C")
val BMWtchbov_fac_FadeDec_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchbov_fac_FadeDec_T")
val BMWtchbov_fac_mf_ThrEcoMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbov_fac_mf_ThrEcoMin_C")
val BMWtchbov_fac_mf_ThrMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbov_fac_mf_ThrMin_C")
val BMWtchbov_fac_mf_ThrSptMin_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchbov_fac_mf_ThrSptMin_M")
val BMWtchbov_fac_mf_ThrThrEcoMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbov_fac_mf_ThrThrEcoMin_C")
val BMWtchbov_fac_mf_ThrThrMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbov_fac_mf_ThrThrMin_C")
val BMWtchbov_fac_mf_ThrThrSptMin_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchbov_fac_mf_ThrThrSptMin_M")
val BMWtchbov_fac_mf_VvtEcoMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbov_fac_mf_VvtEcoMin_C")
val BMWtchbov_fac_mf_VvtMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbov_fac_mf_VvtMin_C")
val BMWtchbov_fac_mf_VvtSptMin_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchbov_fac_mf_VvtSptMin_M")
val BMWtchbov_mf_PmpLim_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchbov_mf_PmpLim_M")
val BMWtchbov_rat_p_CmprThdVlvOp_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbov_rat_p_CmprThdVlvOp_C")
val BMWtchbov_swi_CylOff0Flow_C: String = a2lBin.readCharacteristicWithCast("BMWtchbov_swi_CylOff0Flow_C")
val BMWtchbov_swi_TiRmp_C: String = a2lBin.readCharacteristicWithCast("BMWtchbov_swi_TiRmp_C")
val BMWtchbov_swi_VlvOpAll_C: String = a2lBin.readCharacteristicWithCast("BMWtchbov_swi_VlvOpAll_C")
val BMWtchbov_ti_HldFlow_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchbov_ti_HldFlow_T")
val BMWtchbov_ti_ToutIgTqAcv_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbov_ti_ToutIgTqAcv_C")
val BMWeisy_p_PreThrPlau_uw: InMeasurement = a2lBin.measurement("BMWeisy_p_PreThrPlau_uw")
val BMWosc_fac_DfcoDrvgSit_ub: InMeasurement = a2lBin.measurement("BMWosc_fac_DfcoDrvgSit_ub")
val BMWtchbas_p_BefCmpr_uw: InMeasurement = a2lBin.measurement("BMWtchbas_p_BefCmpr_uw")
val BMWtchbov_b_Acv_bo: InMeasurement = a2lBin.measurement("BMWtchbov_b_Acv_bo")
val BMWtchbov_st_Opm_ub: InMeasurement = a2lBin.measurement("BMWtchbov_st_Opm_ub")
val BMWtchco_b_Clc10MilliSec_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Clc10MilliSec_bo")
val BMWtchsp_p_DifIco_uw: InMeasurement = a2lBin.measurement("BMWtchsp_p_DifIco_uw")
val BMWtqe_b_IgTqAcv_bo: InMeasurement = a2lBin.measurement("BMWtqe_b_IgTqAcv_bo")
val Nkw: InMeasurement = a2lBin.measurement("Nkw")
val St_dps: InMeasurement = a2lBin.measurement("St_dps")
val St_dps_B_gd: InMeasurement = a2lBin.measurement("St_dps.B_gd")
val V_can: InMeasurement = a2lBin.measurement("V_can")
val BMWtchbov_b_CylOffEna_bo: OutMeasurement = a2lBin.measurement("BMWtchbov_b_CylOffEna_bo")
val BMWtchbov_b_VlvOp_bo: OutMeasurement = a2lBin.measurement("BMWtchbov_b_VlvOp_bo")
val BMWtchbov_mf_ThrMin_uw: OutMeasurement = a2lBin.measurement("BMWtchbov_mf_ThrMin_uw")
val BMWtchbov_mf_VvtMin_uw: OutMeasurement = a2lBin.measurement("BMWtchbov_mf_VvtMin_uw")

  BMW_MOD_TchBov_Mf_10ms(BMWeisy_p_PreThrPlau_uw, BMWosc_fac_DfcoDrvgSit_ub, BMWtchbas_p_BefCmpr_uw, BMWtchbov_b_Acv_bo, BMWtchbov_st_Opm_ub, BMWtchco_b_Clc10MilliSec_bo, BMWtchsp_p_DifIco_uw, BMWtqe_b_IgTqAcv_bo, Nkw, St_dps, St_dps_B_gd, V_can, BMWtchbov_cw_CylOffEna_C, BMWtchbov_fac_FadeDec_T, BMWtchbov_fac_mf_ThrEcoMin_C, BMWtchbov_fac_mf_ThrMin_C, BMWtchbov_fac_mf_ThrSptMin_M, BMWtchbov_fac_mf_ThrThrEcoMin_C, BMWtchbov_fac_mf_ThrThrMin_C, BMWtchbov_fac_mf_ThrThrSptMin_M, BMWtchbov_fac_mf_VvtEcoMin_C, BMWtchbov_fac_mf_VvtMin_C, BMWtchbov_fac_mf_VvtSptMin_M, BMWtchbov_mf_PmpLim_M, BMWtchbov_rat_p_CmprThdVlvOp_C, BMWtchbov_swi_CylOff0Flow_C, BMWtchbov_swi_TiRmp_C, BMWtchbov_swi_VlvOpAll_C, BMWtchbov_ti_HldFlow_T, BMWtchbov_ti_ToutIgTqAcv_C, BMWtchbov_b_CylOffEna_bo, BMWtchbov_b_VlvOp_bo, BMWtchbov_mf_ThrMin_uw, BMWtchbov_mf_VvtMin_uw)
}


def BMW_MOD_TchBpi_100ms(BMWeisy_b_PSnsrDblErr_bo: InMeasurement, BMWeisy_b_PSnsrPreThrErr_bo: InMeasurement, BMWeisy_p_PreThrPlau_uw: InMeasurement, BMWtchbpi_fac_P2Pct_uw: InMeasurement, Pumg: InMeasurement, St_kl15: InMeasurement, St_kl15_B_dc_new: InMeasurement, St_kl15_B_kl15_ep: InMeasurement, St_kl15_B_kl15_vorab: InMeasurement, BMWtchbpi_fac_Lpf_C: BigDecimal, BMWtchbpi_p_TcSptDisp_ub: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchBpi_100ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchbpi_fac_Lpf_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbpi_fac_Lpf_C")
val BMWeisy_b_PSnsrDblErr_bo: InMeasurement = a2lBin.measurement("BMWeisy_b_PSnsrDblErr_bo")
val BMWeisy_b_PSnsrPreThrErr_bo: InMeasurement = a2lBin.measurement("BMWeisy_b_PSnsrPreThrErr_bo")
val BMWeisy_p_PreThrPlau_uw: InMeasurement = a2lBin.measurement("BMWeisy_p_PreThrPlau_uw")
val BMWtchbpi_fac_P2Pct_uw: InMeasurement = a2lBin.measurement("BMWtchbpi_fac_P2Pct_uw")
val Pumg: InMeasurement = a2lBin.measurement("Pumg")
val St_kl15: InMeasurement = a2lBin.measurement("St_kl15")
val St_kl15_B_dc_new: InMeasurement = a2lBin.measurement("St_kl15.B_dc_new")
val St_kl15_B_kl15_ep: InMeasurement = a2lBin.measurement("St_kl15.B_kl15_ep")
val St_kl15_B_kl15_vorab: InMeasurement = a2lBin.measurement("St_kl15.B_kl15_vorab")
val BMWtchbpi_p_TcSptDisp_ub: OutMeasurement = a2lBin.measurement("BMWtchbpi_p_TcSptDisp_ub")

  BMW_MOD_TchBpi_100ms(BMWeisy_b_PSnsrDblErr_bo, BMWeisy_b_PSnsrPreThrErr_bo, BMWeisy_p_PreThrPlau_uw, BMWtchbpi_fac_P2Pct_uw, Pumg, St_kl15, St_kl15_B_dc_new, St_kl15_B_kl15_ep, St_kl15_B_kl15_vorab, BMWtchbpi_fac_Lpf_C, BMWtchbpi_p_TcSptDisp_ub)
}


def BMW_MOD_TchBpi_swini(BMWtchbpi_p_MaxTcSptDisp_C: BigDecimal, BMWtchbpi_fac_P2Pct_uw: OutMeasurement, BMWtchbpi_p_MaxTcSptDisp_ub: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchBpi_swini(a2lBin: A2LBinAdapter): Unit = {

val BMWtchbpi_p_MaxTcSptDisp_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchbpi_p_MaxTcSptDisp_C")

val BMWtchbpi_fac_P2Pct_uw: OutMeasurement = a2lBin.measurement("BMWtchbpi_fac_P2Pct_uw")
val BMWtchbpi_p_MaxTcSptDisp_ub: OutMeasurement = a2lBin.measurement("BMWtchbpi_p_MaxTcSptDisp_ub")

  BMW_MOD_TchBpi_swini(BMWtchbpi_p_MaxTcSptDisp_C, BMWtchbpi_fac_P2Pct_uw, BMWtchbpi_p_MaxTcSptDisp_ub)
}


def BMW_MOD_TchCo_Acv_10ms(BMWchas_stb_2_ub: InMeasurement, BMWchas_stb_2_ub_BMWchas_b_DtOpInj2_bc: InMeasurement, BMWchas_stb_2_ub_BMWchas_b_GearAcv_bc: InMeasurement, BMWchas_stb_2_ub_BMWchas_b_Idl_bc: InMeasurement, BMWchas_stb_2_ub_BMWchas_b_Inj2Did_bc: InMeasurement, BMWchas_stb_2_ub_BMWchas_b_Inj2Ena_bc: InMeasurement, BMWchas_stb_2_ub_BMWchas_b_Inj3Ena_bc: InMeasurement, BMWchc_b_WgOpIdl_bo: InMeasurement, BMWchpf_b_OpmTc_bo: InMeasurement, BMWeng_b_StrEnd_bo: InMeasurement, BMWtchbas_b_FlGc_bo: InMeasurement, BMWtchbas_b_Noise_bo: InMeasurement, BMWtchbas_b_RaceStr_bo: InMeasurement, BMWtchbas_p_TcBasc_uw: InMeasurement, BMWtchco_b_Clc10MilliSec_bo: InMeasurement, BMWtchco_b_LnkAftCmprSlv_bo: InMeasurement, BMWtchdiag_b_CtlrOff_bo: InMeasurement, Pld_soll: InMeasurement, Pssol: InMeasurement, Pumg: InMeasurement, BMWtchco_fac_AcvPlOn_C: BigDecimal, BMWtchco_fac_FadePl_T: CurveType[BigDecimal, BigDecimal], BMWtchco_fac_FilFadePl_C: BigDecimal, BMWtchco_p_HysAcv_C: BigDecimal, BMWtchco_swi_AcvPl_C: String, BMWtchco_swi_ClcCtlr_C: String, BMWtchco_swi_ClcPl_C: String, BMWtchco_swi_FlGcCtlrOn_C: String, BMWtchco_ti_DlyCtlrOff_C: BigDecimal, BMWtchsp_swi_PSpRace_C: String, BMWtchco_b_AcvPl_bo: OutMeasurement, BMWtchco_b_Acv_bo: OutMeasurement, BMWtchco_b_ClcCtlr_bo: OutMeasurement, BMWtchco_b_ClcPl_bo: OutMeasurement, BMWtchco_b_CthIdl_bo: OutMeasurement, BMWtchco_b_Rdy_bo: OutMeasurement, BMWtchco_b_Req_bo: OutMeasurement, BMWtchco_fac_FadePl_ub: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchCo_Acv_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchco_fac_AcvPlOn_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchco_fac_AcvPlOn_C")
val BMWtchco_fac_FadePl_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchco_fac_FadePl_T")
val BMWtchco_fac_FilFadePl_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchco_fac_FilFadePl_C")
val BMWtchco_p_HysAcv_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchco_p_HysAcv_C")
val BMWtchco_swi_AcvPl_C: String = a2lBin.readCharacteristicWithCast("BMWtchco_swi_AcvPl_C")
val BMWtchco_swi_ClcCtlr_C: String = a2lBin.readCharacteristicWithCast("BMWtchco_swi_ClcCtlr_C")
val BMWtchco_swi_ClcPl_C: String = a2lBin.readCharacteristicWithCast("BMWtchco_swi_ClcPl_C")
val BMWtchco_swi_FlGcCtlrOn_C: String = a2lBin.readCharacteristicWithCast("BMWtchco_swi_FlGcCtlrOn_C")
val BMWtchco_ti_DlyCtlrOff_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchco_ti_DlyCtlrOff_C")
val BMWtchsp_swi_PSpRace_C: String = a2lBin.readCharacteristicWithCast("BMWtchsp_swi_PSpRace_C")
val BMWchas_stb_2_ub: InMeasurement = a2lBin.measurement("BMWchas_stb_2_ub")
val BMWchas_stb_2_ub_BMWchas_b_DtOpInj2_bc: InMeasurement = a2lBin.measurement("BMWchas_stb_2_ub.BMWchas_b_DtOpInj2_bc")
val BMWchas_stb_2_ub_BMWchas_b_GearAcv_bc: InMeasurement = a2lBin.measurement("BMWchas_stb_2_ub.BMWchas_b_GearAcv_bc")
val BMWchas_stb_2_ub_BMWchas_b_Idl_bc: InMeasurement = a2lBin.measurement("BMWchas_stb_2_ub.BMWchas_b_Idl_bc")
val BMWchas_stb_2_ub_BMWchas_b_Inj2Did_bc: InMeasurement = a2lBin.measurement("BMWchas_stb_2_ub.BMWchas_b_Inj2Did_bc")
val BMWchas_stb_2_ub_BMWchas_b_Inj2Ena_bc: InMeasurement = a2lBin.measurement("BMWchas_stb_2_ub.BMWchas_b_Inj2Ena_bc")
val BMWchas_stb_2_ub_BMWchas_b_Inj3Ena_bc: InMeasurement = a2lBin.measurement("BMWchas_stb_2_ub.BMWchas_b_Inj3Ena_bc")
val BMWchc_b_WgOpIdl_bo: InMeasurement = a2lBin.measurement("BMWchc_b_WgOpIdl_bo")
val BMWchpf_b_OpmTc_bo: InMeasurement = a2lBin.measurement("BMWchpf_b_OpmTc_bo")
val BMWeng_b_StrEnd_bo: InMeasurement = a2lBin.measurement("BMWeng_b_StrEnd_bo")
val BMWtchbas_b_FlGc_bo: InMeasurement = a2lBin.measurement("BMWtchbas_b_FlGc_bo")
val BMWtchbas_b_Noise_bo: InMeasurement = a2lBin.measurement("BMWtchbas_b_Noise_bo")
val BMWtchbas_b_RaceStr_bo: InMeasurement = a2lBin.measurement("BMWtchbas_b_RaceStr_bo")
val BMWtchbas_p_TcBasc_uw: InMeasurement = a2lBin.measurement("BMWtchbas_p_TcBasc_uw")
val BMWtchco_b_Clc10MilliSec_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Clc10MilliSec_bo")
val BMWtchco_b_LnkAftCmprSlv_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_LnkAftCmprSlv_bo")
val BMWtchdiag_b_CtlrOff_bo: InMeasurement = a2lBin.measurement("BMWtchdiag_b_CtlrOff_bo")
val Pld_soll: InMeasurement = a2lBin.measurement("Pld_soll")
val Pssol: InMeasurement = a2lBin.measurement("Pssol")
val Pumg: InMeasurement = a2lBin.measurement("Pumg")
val BMWtchco_b_AcvPl_bo: OutMeasurement = a2lBin.measurement("BMWtchco_b_AcvPl_bo")
val BMWtchco_b_Acv_bo: OutMeasurement = a2lBin.measurement("BMWtchco_b_Acv_bo")
val BMWtchco_b_ClcCtlr_bo: OutMeasurement = a2lBin.measurement("BMWtchco_b_ClcCtlr_bo")
val BMWtchco_b_ClcPl_bo: OutMeasurement = a2lBin.measurement("BMWtchco_b_ClcPl_bo")
val BMWtchco_b_CthIdl_bo: OutMeasurement = a2lBin.measurement("BMWtchco_b_CthIdl_bo")
val BMWtchco_b_Rdy_bo: OutMeasurement = a2lBin.measurement("BMWtchco_b_Rdy_bo")
val BMWtchco_b_Req_bo: OutMeasurement = a2lBin.measurement("BMWtchco_b_Req_bo")
val BMWtchco_fac_FadePl_ub: OutMeasurement = a2lBin.measurement("BMWtchco_fac_FadePl_ub")

  BMW_MOD_TchCo_Acv_10ms(BMWchas_stb_2_ub, BMWchas_stb_2_ub_BMWchas_b_DtOpInj2_bc, BMWchas_stb_2_ub_BMWchas_b_GearAcv_bc, BMWchas_stb_2_ub_BMWchas_b_Idl_bc, BMWchas_stb_2_ub_BMWchas_b_Inj2Did_bc, BMWchas_stb_2_ub_BMWchas_b_Inj2Ena_bc, BMWchas_stb_2_ub_BMWchas_b_Inj3Ena_bc, BMWchc_b_WgOpIdl_bo, BMWchpf_b_OpmTc_bo, BMWeng_b_StrEnd_bo, BMWtchbas_b_FlGc_bo, BMWtchbas_b_Noise_bo, BMWtchbas_b_RaceStr_bo, BMWtchbas_p_TcBasc_uw, BMWtchco_b_Clc10MilliSec_bo, BMWtchco_b_LnkAftCmprSlv_bo, BMWtchdiag_b_CtlrOff_bo, Pld_soll, Pssol, Pumg, BMWtchco_fac_AcvPlOn_C, BMWtchco_fac_FadePl_T, BMWtchco_fac_FilFadePl_C, BMWtchco_p_HysAcv_C, BMWtchco_swi_AcvPl_C, BMWtchco_swi_ClcCtlr_C, BMWtchco_swi_ClcPl_C, BMWtchco_swi_FlGcCtlrOn_C, BMWtchco_ti_DlyCtlrOff_C, BMWtchsp_swi_PSpRace_C, BMWtchco_b_AcvPl_bo, BMWtchco_b_Acv_bo, BMWtchco_b_ClcCtlr_bo, BMWtchco_b_ClcPl_bo, BMWtchco_b_CthIdl_bo, BMWtchco_b_Rdy_bo, BMWtchco_b_Req_bo, BMWtchco_fac_FadePl_ub)
}


def BMW_MOD_TchCo_Opm_10ms(BMWeisy_b_PSnsrPreThrPlau_bo: InMeasurement, BMWeng_b_CkMovg_bo: InMeasurement, BMWls_b_CondRiskIco_bo: InMeasurement, BMWtchco_b_Acv_bo: InMeasurement, BMWtchco_b_Clc10MilliSec_bo: InMeasurement, BMWtqe_b_Dfco_bo: InMeasurement, Pwg_ist: InMeasurement, St_bgkuppl1: InMeasurement, St_bgkuppl1_B_ll: InMeasurement, St_dsc_can: InMeasurement, St_fgr: InMeasurement, St_fgr_B_fgr_akt: InMeasurement, St_fgr_B_sport_aktiv: InMeasurement, St_fw: InMeasurement, St_fw_B_hschalt_komb: InMeasurement, St_fw_B_ldm_akt04: InMeasurement, St_fw_B_ldm_off: InMeasurement, St_fw_B_ldm_offhs: InMeasurement, St_fw_B_ldm_offreg: InMeasurement, St_fw_B_ldm_offrs: InMeasurement, St_fw_B_rschalt_komb: InMeasurement, St_poscab: InMeasurement, Stat_vmbm: InMeasurement, Status_usecase_antr: InMeasurement, Toel: InMeasurement, V_can: InMeasurement, BMWtchco_cw_1_C: BigDecimal, BMWtchco_cw_DscSpt_C: BigDecimal, BMWtchco_cw_RoofOp_C: BigDecimal, BMWtchco_rat_AccrEcoOff_C: BigDecimal, BMWtchco_rat_AccrSpt_C: BigDecimal, BMWtchco_rat_gra_AccrEcoOff_C: BigDecimal, BMWtchco_rat_gra_AccrEcoOn_C: BigDecimal, BMWtchco_rat_gra_AccrSpt_C: BigDecimal, BMWtchco_st_OpmMap_Ca: Array[String], BMWtchco_t_OilEco_C: BigDecimal, BMWtchco_ti_DlyAccrEcoOn_C: BigDecimal, BMWtchco_ti_DlyAccrSpt_C: BigDecimal, BMWtchco_v_OpmRoofOpHys_C: BigDecimal, BMWtchco_v_OpmRoofOpMax_C: BigDecimal, BMWtchco_v_OpmRoofOpMin_C: BigDecimal, BMWtchco_v_OpmSptHys_C: BigDecimal, BMWtchco_v_OpmSptMax_C: BigDecimal, BMWtchco_v_OpmSptMin_C: BigDecimal, BMWtchco_st_Opm_ub: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchCo_Opm_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchco_cw_1_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchco_cw_1_C")
val BMWtchco_cw_DscSpt_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchco_cw_DscSpt_C")
val BMWtchco_cw_RoofOp_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchco_cw_RoofOp_C")
val BMWtchco_rat_AccrEcoOff_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchco_rat_AccrEcoOff_C")
val BMWtchco_rat_AccrSpt_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchco_rat_AccrSpt_C")
val BMWtchco_rat_gra_AccrEcoOff_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchco_rat_gra_AccrEcoOff_C")
val BMWtchco_rat_gra_AccrEcoOn_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchco_rat_gra_AccrEcoOn_C")
val BMWtchco_rat_gra_AccrSpt_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchco_rat_gra_AccrSpt_C")
val BMWtchco_st_OpmMap_Ca: Array[String] = a2lBin.readCharacteristicWithCast("BMWtchco_st_OpmMap_Ca")
val BMWtchco_t_OilEco_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchco_t_OilEco_C")
val BMWtchco_ti_DlyAccrEcoOn_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchco_ti_DlyAccrEcoOn_C")
val BMWtchco_ti_DlyAccrSpt_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchco_ti_DlyAccrSpt_C")
val BMWtchco_v_OpmRoofOpHys_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchco_v_OpmRoofOpHys_C")
val BMWtchco_v_OpmRoofOpMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchco_v_OpmRoofOpMax_C")
val BMWtchco_v_OpmRoofOpMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchco_v_OpmRoofOpMin_C")
val BMWtchco_v_OpmSptHys_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchco_v_OpmSptHys_C")
val BMWtchco_v_OpmSptMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchco_v_OpmSptMax_C")
val BMWtchco_v_OpmSptMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchco_v_OpmSptMin_C")
val BMWeisy_b_PSnsrPreThrPlau_bo: InMeasurement = a2lBin.measurement("BMWeisy_b_PSnsrPreThrPlau_bo")
val BMWeng_b_CkMovg_bo: InMeasurement = a2lBin.measurement("BMWeng_b_CkMovg_bo")
val BMWls_b_CondRiskIco_bo: InMeasurement = a2lBin.measurement("BMWls_b_CondRiskIco_bo")
val BMWtchco_b_Acv_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Acv_bo")
val BMWtchco_b_Clc10MilliSec_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Clc10MilliSec_bo")
val BMWtqe_b_Dfco_bo: InMeasurement = a2lBin.measurement("BMWtqe_b_Dfco_bo")
val Pwg_ist: InMeasurement = a2lBin.measurement("Pwg_ist")
val St_bgkuppl1: InMeasurement = a2lBin.measurement("St_bgkuppl1")
val St_bgkuppl1_B_ll: InMeasurement = a2lBin.measurement("St_bgkuppl1.B_ll")
val St_dsc_can: InMeasurement = a2lBin.measurement("St_dsc_can")
val St_fgr: InMeasurement = a2lBin.measurement("St_fgr")
val St_fgr_B_fgr_akt: InMeasurement = a2lBin.measurement("St_fgr.B_fgr_akt")
val St_fgr_B_sport_aktiv: InMeasurement = a2lBin.measurement("St_fgr.B_sport_aktiv")
val St_fw: InMeasurement = a2lBin.measurement("St_fw")
val St_fw_B_hschalt_komb: InMeasurement = a2lBin.measurement("St_fw.B_hschalt_komb")
val St_fw_B_ldm_akt04: InMeasurement = a2lBin.measurement("St_fw.B_ldm_akt04")
val St_fw_B_ldm_off: InMeasurement = a2lBin.measurement("St_fw.B_ldm_off")
val St_fw_B_ldm_offhs: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offhs")
val St_fw_B_ldm_offreg: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offreg")
val St_fw_B_ldm_offrs: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offrs")
val St_fw_B_rschalt_komb: InMeasurement = a2lBin.measurement("St_fw.B_rschalt_komb")
val St_poscab: InMeasurement = a2lBin.measurement("St_poscab")
val Stat_vmbm: InMeasurement = a2lBin.measurement("Stat_vmbm")
val Status_usecase_antr: InMeasurement = a2lBin.measurement("Status_usecase_antr")
val Toel: InMeasurement = a2lBin.measurement("Toel")
val V_can: InMeasurement = a2lBin.measurement("V_can")
val BMWtchco_st_Opm_ub: OutMeasurement = a2lBin.measurement("BMWtchco_st_Opm_ub")

  BMW_MOD_TchCo_Opm_10ms(BMWeisy_b_PSnsrPreThrPlau_bo, BMWeng_b_CkMovg_bo, BMWls_b_CondRiskIco_bo, BMWtchco_b_Acv_bo, BMWtchco_b_Clc10MilliSec_bo, BMWtqe_b_Dfco_bo, Pwg_ist, St_bgkuppl1, St_bgkuppl1_B_ll, St_dsc_can, St_fgr, St_fgr_B_fgr_akt, St_fgr_B_sport_aktiv, St_fw, St_fw_B_hschalt_komb, St_fw_B_ldm_akt04, St_fw_B_ldm_off, St_fw_B_ldm_offhs, St_fw_B_ldm_offreg, St_fw_B_ldm_offrs, St_fw_B_rschalt_komb, St_poscab, Stat_vmbm, Status_usecase_antr, Toel, V_can, BMWtchco_cw_1_C, BMWtchco_cw_DscSpt_C, BMWtchco_cw_RoofOp_C, BMWtchco_rat_AccrEcoOff_C, BMWtchco_rat_AccrSpt_C, BMWtchco_rat_gra_AccrEcoOff_C, BMWtchco_rat_gra_AccrEcoOn_C, BMWtchco_rat_gra_AccrSpt_C, BMWtchco_st_OpmMap_Ca, BMWtchco_t_OilEco_C, BMWtchco_ti_DlyAccrEcoOn_C, BMWtchco_ti_DlyAccrSpt_C, BMWtchco_v_OpmRoofOpHys_C, BMWtchco_v_OpmRoofOpMax_C, BMWtchco_v_OpmRoofOpMin_C, BMWtchco_v_OpmSptHys_C, BMWtchco_v_OpmSptMax_C, BMWtchco_v_OpmSptMin_C, BMWtchco_st_Opm_ub)
}


def BMW_MOD_TchCo_Pam_10ms(Tic10ms: InMeasurement, BMWtchco_cw_Clc10MilliSe2_C: BigDecimal, BMWtchco_cw_Clc10MilliSe_C: BigDecimal, BMWtchco_b_Clc10MilliSec2_bo: OutMeasurement, BMWtchco_b_Clc10MilliSec_bo: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchCo_Pam_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchco_cw_Clc10MilliSe2_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchco_cw_Clc10MilliSe2_C")
val BMWtchco_cw_Clc10MilliSe_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchco_cw_Clc10MilliSe_C")
val Tic10ms: InMeasurement = a2lBin.measurement("Tic10ms")
val BMWtchco_b_Clc10MilliSec2_bo: OutMeasurement = a2lBin.measurement("BMWtchco_b_Clc10MilliSec2_bo")
val BMWtchco_b_Clc10MilliSec_bo: OutMeasurement = a2lBin.measurement("BMWtchco_b_Clc10MilliSec_bo")

  BMW_MOD_TchCo_Pam_10ms(Tic10ms, BMWtchco_cw_Clc10MilliSe2_C, BMWtchco_cw_Clc10MilliSe_C, BMWtchco_b_Clc10MilliSec2_bo, BMWtchco_b_Clc10MilliSec_bo)
}


def BMW_MOD_TchCo_swini(BMWtchco_b_LnkAftCmprSlv_bo: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchCo_swini(a2lBin: A2LBinAdapter): Unit = {



val BMWtchco_b_LnkAftCmprSlv_bo: OutMeasurement = a2lBin.measurement("BMWtchco_b_LnkAftCmprSlv_bo")

  BMW_MOD_TchCo_swini(BMWtchco_b_LnkAftCmprSlv_bo)
}


def BMW_MOD_TchCtr_100ms(Fho: InMeasurement, Pak: InMeasurement, Tumg: InMeasurement, V_can: InMeasurement, BMWtchctr_fac_AltiCrtn_T: CurveType[BigDecimal, BigDecimal], BMWtchctr_pct_WgAltiCrtn_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchctr_v_WgClsdPlHys_C: BigDecimal, BMWtchctr_v_WgClsdPlMax_C: BigDecimal, BMWtchctr_v_WgClsdPlMin_C: BigDecimal, BMWtchctr_b_WgClsdPl_bo: OutMeasurement, BMWtchctr_pct_WgAltiCrtn_sw: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchCtr_100ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchctr_fac_AltiCrtn_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_AltiCrtn_T")
val BMWtchctr_pct_WgAltiCrtn_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_pct_WgAltiCrtn_M")
val BMWtchctr_v_WgClsdPlHys_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_v_WgClsdPlHys_C")
val BMWtchctr_v_WgClsdPlMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_v_WgClsdPlMax_C")
val BMWtchctr_v_WgClsdPlMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_v_WgClsdPlMin_C")
val Fho: InMeasurement = a2lBin.measurement("Fho")
val Pak: InMeasurement = a2lBin.measurement("Pak")
val Tumg: InMeasurement = a2lBin.measurement("Tumg")
val V_can: InMeasurement = a2lBin.measurement("V_can")
val BMWtchctr_b_WgClsdPl_bo: OutMeasurement = a2lBin.measurement("BMWtchctr_b_WgClsdPl_bo")
val BMWtchctr_pct_WgAltiCrtn_sw: OutMeasurement = a2lBin.measurement("BMWtchctr_pct_WgAltiCrtn_sw")

  BMW_MOD_TchCtr_100ms(Fho, Pak, Tumg, V_can, BMWtchctr_fac_AltiCrtn_T, BMWtchctr_pct_WgAltiCrtn_M, BMWtchctr_v_WgClsdPlHys_C, BMWtchctr_v_WgClsdPlMax_C, BMWtchctr_v_WgClsdPlMin_C, BMWtchctr_b_WgClsdPl_bo, BMWtchctr_pct_WgAltiCrtn_sw)
}


def BMW_MOD_TchCtr_PosAdIp_10ms(BMWtchad_fac_Raw_uw: InMeasurement, BMWtchbas_b_FlGcHld_bo: InMeasurement, BMWtchbas_n_Dyn_sw: InMeasurement, BMWtchbas_p_BefThrDyn_sw: InMeasurement, BMWtchbas_p_Dif_sw: InMeasurement, BMWtchbas_rat_p_BascBefCmpr_uw: InMeasurement, BMWtchco_b_AcvPl_bo: InMeasurement, BMWtchco_b_Acv_bo: InMeasurement, BMWtchco_b_Clc10MilliSec_bo: InMeasurement, BMWtchctr_pct_WgAct_uw: InMeasurement, BMWtchctr_pct_WgCrtd1_uw: InMeasurement, BMWtchctr_pct_WgCrtd3_uw: InMeasurement, BMWtchdiag_b_LimPctl_bo: InMeasurement, BMWtchsp_p_ReqDyn_sw: InMeasurement, BMWtchsp_rat_p_Cmpr_uw: InMeasurement, Nkw: InMeasurement, BMWtchctr_fac_FadeAdpMax_T: CurveType[BigDecimal, BigDecimal], BMWtchctr_fac_FadeAdp_T: CurveType[BigDecimal, BigDecimal], BMWtchctr_fac_IpPctWg_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchctr_n_DynIpRstMax_C: BigDecimal, BMWtchctr_n_DynIpRstMin_C: BigDecimal, BMWtchctr_p_BefThrDynIpStop_C: BigDecimal, BMWtchctr_p_DifIpRstMax_C: BigDecimal, BMWtchctr_p_DifIpRstMin_C: BigDecimal, BMWtchctr_p_DynIpRstMax_C: BigDecimal, BMWtchctr_p_DynIpRstMin_C: BigDecimal, BMWtchctr_p_ReqDynIpStop_C: BigDecimal, BMWtchctr_pct_WgIpLimMax_T: CurveType[BigDecimal, BigDecimal], BMWtchctr_pct_WgIpLimMin_T: CurveType[BigDecimal, BigDecimal], BMWtchctr_pct_WgIpLimPlMin_C: BigDecimal, BMWtchctr_pct_WgIpMan_C: BigDecimal, BMWtchctr_pct_WgIpRstMax_C: BigDecimal, BMWtchctr_pct_WgIpRstMin_C: BigDecimal, BMWtchctr_pct_WgPosnIpStopMax_C: BigDecimal, BMWtchctr_pct_WgPosnIpStopMin_C: BigDecimal, BMWtchctr_pct_WgSpIpStopMax_C: BigDecimal, BMWtchctr_swi_IpFcdOn_C: String, BMWtchctr_swi_IpRstMan_C: String, BMWtchctr_swi_WgIpMan_C: String, BMWtchco_swi_FlGcCtlrOn_C: String, BMWtchctr_swi_IpPwrAcvn_C: String, BMWtchctr_b_IpRst_bo: OutMeasurement, BMWtchctr_b_IpStop_bo: OutMeasurement, BMWtchctr_fac_FadeAdp_ub: OutMeasurement, BMWtchctr_pct_WgBefLim_uw: OutMeasurement, BMWtchctr_pct_WgIp_sw: OutMeasurement, BMWtchctr_pct_WgLimMax_uw: OutMeasurement, BMWtchctr_pct_Wg_uw: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchCtr_PosAdIp_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchctr_fac_FadeAdpMax_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_FadeAdpMax_T")
val BMWtchctr_fac_FadeAdp_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_FadeAdp_T")
val BMWtchctr_fac_IpPctWg_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_IpPctWg_M")
val BMWtchctr_n_DynIpRstMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_n_DynIpRstMax_C")
val BMWtchctr_n_DynIpRstMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_n_DynIpRstMin_C")
val BMWtchctr_p_BefThrDynIpStop_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_p_BefThrDynIpStop_C")
val BMWtchctr_p_DifIpRstMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_p_DifIpRstMax_C")
val BMWtchctr_p_DifIpRstMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_p_DifIpRstMin_C")
val BMWtchctr_p_DynIpRstMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_p_DynIpRstMax_C")
val BMWtchctr_p_DynIpRstMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_p_DynIpRstMin_C")
val BMWtchctr_p_ReqDynIpStop_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_p_ReqDynIpStop_C")
val BMWtchctr_pct_WgIpLimMax_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_pct_WgIpLimMax_T")
val BMWtchctr_pct_WgIpLimMin_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_pct_WgIpLimMin_T")
val BMWtchctr_pct_WgIpLimPlMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_pct_WgIpLimPlMin_C")
val BMWtchctr_pct_WgIpMan_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_pct_WgIpMan_C")
val BMWtchctr_pct_WgIpRstMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_pct_WgIpRstMax_C")
val BMWtchctr_pct_WgIpRstMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_pct_WgIpRstMin_C")
val BMWtchctr_pct_WgPosnIpStopMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_pct_WgPosnIpStopMax_C")
val BMWtchctr_pct_WgPosnIpStopMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_pct_WgPosnIpStopMin_C")
val BMWtchctr_pct_WgSpIpStopMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_pct_WgSpIpStopMax_C")
val BMWtchctr_swi_IpFcdOn_C: String = a2lBin.readCharacteristicWithCast("BMWtchctr_swi_IpFcdOn_C")
val BMWtchctr_swi_IpRstMan_C: String = a2lBin.readCharacteristicWithCast("BMWtchctr_swi_IpRstMan_C")
val BMWtchctr_swi_WgIpMan_C: String = a2lBin.readCharacteristicWithCast("BMWtchctr_swi_WgIpMan_C")
val BMWtchco_swi_FlGcCtlrOn_C: String = a2lBin.readCharacteristicWithCast("BMWtchco_swi_FlGcCtlrOn_C")
val BMWtchctr_swi_IpPwrAcvn_C: String = a2lBin.readCharacteristicWithCast("BMWtchctr_swi_IpPwrAcvn_C")
val BMWtchad_fac_Raw_uw: InMeasurement = a2lBin.measurement("BMWtchad_fac_Raw_uw")
val BMWtchbas_b_FlGcHld_bo: InMeasurement = a2lBin.measurement("BMWtchbas_b_FlGcHld_bo")
val BMWtchbas_n_Dyn_sw: InMeasurement = a2lBin.measurement("BMWtchbas_n_Dyn_sw")
val BMWtchbas_p_BefThrDyn_sw: InMeasurement = a2lBin.measurement("BMWtchbas_p_BefThrDyn_sw")
val BMWtchbas_p_Dif_sw: InMeasurement = a2lBin.measurement("BMWtchbas_p_Dif_sw")
val BMWtchbas_rat_p_BascBefCmpr_uw: InMeasurement = a2lBin.measurement("BMWtchbas_rat_p_BascBefCmpr_uw")
val BMWtchco_b_AcvPl_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_AcvPl_bo")
val BMWtchco_b_Acv_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Acv_bo")
val BMWtchco_b_Clc10MilliSec_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Clc10MilliSec_bo")
val BMWtchctr_pct_WgAct_uw: InMeasurement = a2lBin.measurement("BMWtchctr_pct_WgAct_uw")
val BMWtchctr_pct_WgCrtd1_uw: InMeasurement = a2lBin.measurement("BMWtchctr_pct_WgCrtd1_uw")
val BMWtchctr_pct_WgCrtd3_uw: InMeasurement = a2lBin.measurement("BMWtchctr_pct_WgCrtd3_uw")
val BMWtchdiag_b_LimPctl_bo: InMeasurement = a2lBin.measurement("BMWtchdiag_b_LimPctl_bo")
val BMWtchsp_p_ReqDyn_sw: InMeasurement = a2lBin.measurement("BMWtchsp_p_ReqDyn_sw")
val BMWtchsp_rat_p_Cmpr_uw: InMeasurement = a2lBin.measurement("BMWtchsp_rat_p_Cmpr_uw")
val Nkw: InMeasurement = a2lBin.measurement("Nkw")
val BMWtchctr_b_IpRst_bo: OutMeasurement = a2lBin.measurement("BMWtchctr_b_IpRst_bo")
val BMWtchctr_b_IpStop_bo: OutMeasurement = a2lBin.measurement("BMWtchctr_b_IpStop_bo")
val BMWtchctr_fac_FadeAdp_ub: OutMeasurement = a2lBin.measurement("BMWtchctr_fac_FadeAdp_ub")
val BMWtchctr_pct_WgBefLim_uw: OutMeasurement = a2lBin.measurement("BMWtchctr_pct_WgBefLim_uw")
val BMWtchctr_pct_WgIp_sw: OutMeasurement = a2lBin.measurement("BMWtchctr_pct_WgIp_sw")
val BMWtchctr_pct_WgLimMax_uw: OutMeasurement = a2lBin.measurement("BMWtchctr_pct_WgLimMax_uw")
val BMWtchctr_pct_Wg_uw: OutMeasurement = a2lBin.measurement("BMWtchctr_pct_Wg_uw")

  BMW_MOD_TchCtr_PosAdIp_10ms(BMWtchad_fac_Raw_uw, BMWtchbas_b_FlGcHld_bo, BMWtchbas_n_Dyn_sw, BMWtchbas_p_BefThrDyn_sw, BMWtchbas_p_Dif_sw, BMWtchbas_rat_p_BascBefCmpr_uw, BMWtchco_b_AcvPl_bo, BMWtchco_b_Acv_bo, BMWtchco_b_Clc10MilliSec_bo, BMWtchctr_pct_WgAct_uw, BMWtchctr_pct_WgCrtd1_uw, BMWtchctr_pct_WgCrtd3_uw, BMWtchdiag_b_LimPctl_bo, BMWtchsp_p_ReqDyn_sw, BMWtchsp_rat_p_Cmpr_uw, Nkw, BMWtchctr_fac_FadeAdpMax_T, BMWtchctr_fac_FadeAdp_T, BMWtchctr_fac_IpPctWg_M, BMWtchctr_n_DynIpRstMax_C, BMWtchctr_n_DynIpRstMin_C, BMWtchctr_p_BefThrDynIpStop_C, BMWtchctr_p_DifIpRstMax_C, BMWtchctr_p_DifIpRstMin_C, BMWtchctr_p_DynIpRstMax_C, BMWtchctr_p_DynIpRstMin_C, BMWtchctr_p_ReqDynIpStop_C, BMWtchctr_pct_WgIpLimMax_T, BMWtchctr_pct_WgIpLimMin_T, BMWtchctr_pct_WgIpLimPlMin_C, BMWtchctr_pct_WgIpMan_C, BMWtchctr_pct_WgIpRstMax_C, BMWtchctr_pct_WgIpRstMin_C, BMWtchctr_pct_WgPosnIpStopMax_C, BMWtchctr_pct_WgPosnIpStopMin_C, BMWtchctr_pct_WgSpIpStopMax_C, BMWtchctr_swi_IpFcdOn_C, BMWtchctr_swi_IpRstMan_C, BMWtchctr_swi_WgIpMan_C, BMWtchco_swi_FlGcCtlrOn_C, BMWtchctr_swi_IpPwrAcvn_C, BMWtchctr_b_IpRst_bo, BMWtchctr_b_IpStop_bo, BMWtchctr_fac_FadeAdp_ub, BMWtchctr_pct_WgBefLim_uw, BMWtchctr_pct_WgIp_sw, BMWtchctr_pct_WgLimMax_uw, BMWtchctr_pct_Wg_uw)
}


def BMW_MOD_TchCtr_Pos_10ms(BMWtchbas_b_FlGcHld_bo: InMeasurement, BMWtchbas_b_Noise_bo: InMeasurement, BMWtchbas_b_RaceStr_bo: InMeasurement, BMWtchbas_p_DifGra_sw: InMeasurement, BMWtchbas_p_Dif_sw: InMeasurement, BMWtchco_b_AcvPl_bo: InMeasurement, BMWtchco_b_Acv_bo: InMeasurement, BMWtchco_b_Clc10MilliSec_bo: InMeasurement, BMWtchco_b_ClcPl_bo: InMeasurement, BMWtchco_b_Rdy_bo: InMeasurement, BMWtchco_fac_FadePl_ub: InMeasurement, BMWtchco_st_Opm_ub: InMeasurement, BMWtchctr_b_WgClsdPl_bo: InMeasurement, BMWtchctr_pct_WgBasc_uw: InMeasurement, Nkw: InMeasurement, Rf_soll: InMeasurement, BMWtchctr_fac_FilPctWgDyn_C: BigDecimal, BMWtchctr_fac_FilRatWg_T: CurveType[BigDecimal, BigDecimal], BMWtchctr_p_DifGraWgClsd_C: BigDecimal, BMWtchctr_p_DifWgClsd_T: CurveType[BigDecimal, BigDecimal], BMWtchctr_pct_WgClsdMin_C: BigDecimal, BMWtchctr_pct_WgClsdPl_C: BigDecimal, BMWtchctr_pct_WgDeltRaceStr_C: BigDecimal, BMWtchctr_pct_WgDyn_C: BigDecimal, BMWtchctr_pct_WgFlGc_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchctr_pct_WgNoise_T: CurveType[BigDecimal, BigDecimal], BMWtchctr_pct_WgPlCond_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchctr_pct_WgPlEco_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchctr_pct_WgPlRoofOp_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchctr_pct_WgPlSpt_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchctr_pct_WgPl_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchctr_ti_DlyPl_T: CurveType[BigDecimal, BigDecimal], BMWtchctr_ti_DlyWgClsd_T: CurveType[BigDecimal, BigDecimal], BMWtchco_swi_FlGcCtlrOn_C: String, BMWtchctr_pct_WgCrtd1_uw: OutMeasurement, BMWtchctr_pct_WgCrtd3_uw: OutMeasurement, BMWtchctr_pct_WgRaw_uw: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchCtr_Pos_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchctr_fac_FilPctWgDyn_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_FilPctWgDyn_C")
val BMWtchctr_fac_FilRatWg_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_FilRatWg_T")
val BMWtchctr_p_DifGraWgClsd_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_p_DifGraWgClsd_C")
val BMWtchctr_p_DifWgClsd_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_p_DifWgClsd_T")
val BMWtchctr_pct_WgClsdMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_pct_WgClsdMin_C")
val BMWtchctr_pct_WgClsdPl_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_pct_WgClsdPl_C")
val BMWtchctr_pct_WgDeltRaceStr_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_pct_WgDeltRaceStr_C")
val BMWtchctr_pct_WgDyn_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_pct_WgDyn_C")
val BMWtchctr_pct_WgFlGc_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_pct_WgFlGc_M")
val BMWtchctr_pct_WgNoise_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_pct_WgNoise_T")
val BMWtchctr_pct_WgPlCond_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_pct_WgPlCond_M")
val BMWtchctr_pct_WgPlEco_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_pct_WgPlEco_M")
val BMWtchctr_pct_WgPlRoofOp_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_pct_WgPlRoofOp_M")
val BMWtchctr_pct_WgPlSpt_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_pct_WgPlSpt_M")
val BMWtchctr_pct_WgPl_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_pct_WgPl_M")
val BMWtchctr_ti_DlyPl_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_ti_DlyPl_T")
val BMWtchctr_ti_DlyWgClsd_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_ti_DlyWgClsd_T")
val BMWtchco_swi_FlGcCtlrOn_C: String = a2lBin.readCharacteristicWithCast("BMWtchco_swi_FlGcCtlrOn_C")
val BMWtchbas_b_FlGcHld_bo: InMeasurement = a2lBin.measurement("BMWtchbas_b_FlGcHld_bo")
val BMWtchbas_b_Noise_bo: InMeasurement = a2lBin.measurement("BMWtchbas_b_Noise_bo")
val BMWtchbas_b_RaceStr_bo: InMeasurement = a2lBin.measurement("BMWtchbas_b_RaceStr_bo")
val BMWtchbas_p_DifGra_sw: InMeasurement = a2lBin.measurement("BMWtchbas_p_DifGra_sw")
val BMWtchbas_p_Dif_sw: InMeasurement = a2lBin.measurement("BMWtchbas_p_Dif_sw")
val BMWtchco_b_AcvPl_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_AcvPl_bo")
val BMWtchco_b_Acv_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Acv_bo")
val BMWtchco_b_Clc10MilliSec_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Clc10MilliSec_bo")
val BMWtchco_b_ClcPl_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_ClcPl_bo")
val BMWtchco_b_Rdy_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Rdy_bo")
val BMWtchco_fac_FadePl_ub: InMeasurement = a2lBin.measurement("BMWtchco_fac_FadePl_ub")
val BMWtchco_st_Opm_ub: InMeasurement = a2lBin.measurement("BMWtchco_st_Opm_ub")
val BMWtchctr_b_WgClsdPl_bo: InMeasurement = a2lBin.measurement("BMWtchctr_b_WgClsdPl_bo")
val BMWtchctr_pct_WgBasc_uw: InMeasurement = a2lBin.measurement("BMWtchctr_pct_WgBasc_uw")
val Nkw: InMeasurement = a2lBin.measurement("Nkw")
val Rf_soll: InMeasurement = a2lBin.measurement("Rf_soll")
val BMWtchctr_pct_WgCrtd1_uw: OutMeasurement = a2lBin.measurement("BMWtchctr_pct_WgCrtd1_uw")
val BMWtchctr_pct_WgCrtd3_uw: OutMeasurement = a2lBin.measurement("BMWtchctr_pct_WgCrtd3_uw")
val BMWtchctr_pct_WgRaw_uw: OutMeasurement = a2lBin.measurement("BMWtchctr_pct_WgRaw_uw")

  BMW_MOD_TchCtr_Pos_10ms(BMWtchbas_b_FlGcHld_bo, BMWtchbas_b_Noise_bo, BMWtchbas_b_RaceStr_bo, BMWtchbas_p_DifGra_sw, BMWtchbas_p_Dif_sw, BMWtchco_b_AcvPl_bo, BMWtchco_b_Acv_bo, BMWtchco_b_Clc10MilliSec_bo, BMWtchco_b_ClcPl_bo, BMWtchco_b_Rdy_bo, BMWtchco_fac_FadePl_ub, BMWtchco_st_Opm_ub, BMWtchctr_b_WgClsdPl_bo, BMWtchctr_pct_WgBasc_uw, Nkw, Rf_soll, BMWtchctr_fac_FilPctWgDyn_C, BMWtchctr_fac_FilRatWg_T, BMWtchctr_p_DifGraWgClsd_C, BMWtchctr_p_DifWgClsd_T, BMWtchctr_pct_WgClsdMin_C, BMWtchctr_pct_WgClsdPl_C, BMWtchctr_pct_WgDeltRaceStr_C, BMWtchctr_pct_WgDyn_C, BMWtchctr_pct_WgFlGc_M, BMWtchctr_pct_WgNoise_T, BMWtchctr_pct_WgPlCond_M, BMWtchctr_pct_WgPlEco_M, BMWtchctr_pct_WgPlRoofOp_M, BMWtchctr_pct_WgPlSpt_M, BMWtchctr_pct_WgPl_M, BMWtchctr_ti_DlyPl_T, BMWtchctr_ti_DlyWgClsd_T, BMWtchco_swi_FlGcCtlrOn_C, BMWtchctr_pct_WgCrtd1_uw, BMWtchctr_pct_WgCrtd3_uw, BMWtchctr_pct_WgRaw_uw)
}


def BMW_MOD_TchCtr_Pwr2Pos_10ms(BMWbdy_b_EgFlp_bo: InMeasurement, BMWext_t_GasP2PclFilXab_sw: InMeasurement, BMWgpfp_b_PclFil_bo: InMeasurement, BMWgpfp_fac_AdpPclFil_uw: InMeasurement, BMWgpfp_m_SootLoad_sw: InMeasurement, BMWign_ag_OutMv_sw: InMeasurement, BMWtchbas_b_FlGc_bo: InMeasurement, BMWtchbas_p_Dif_sw: InMeasurement, BMWtchco_b_Clc10MilliSec_bo: InMeasurement, BMWtchco_b_ClcCtlr_bo: InMeasurement, BMWtchco_st_Opm_ub: InMeasurement, BMWtchctr_pct_WgAltiCrtn_sw: InMeasurement, BMWtchctr_pct_WgRaw_uw: InMeasurement, BMWtchctr_pwr_Trb_uw: InMeasurement, BMWtchscv_fac__ub: InMeasurement, BMWtchsp_mf_Ex_uw: InMeasurement, BMWtchsp_p_ReqDyn_sw: InMeasurement, Eta_zyl_aus: InMeasurement, Msakzu: InMeasurement, Nkw: InMeasurement, Pumg: InMeasurement, Rf: InMeasurement, St_getrdaten: InMeasurement, St_getrdaten_B_gangwechsel_gs: InMeasurement, Tabg_mw: InMeasurement, Vsa_spri_f: InMeasurement, Zwstat: InMeasurement, BMWtchctr_cpp_ExGas_T: CurveType[BigDecimal, BigDecimal], BMWtchctr_cw_FadeFlowSpSpt_C: BigDecimal, BMWtchctr_eff_CylOffMin_C: BigDecimal, BMWtchctr_fac_FadeFlowSpAcv_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchctr_fac_FadeFlowSpSpt_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchctr_fac_FadeTExGasGc_C: BigDecimal, BMWtchctr_fac_FadeTExGas_C: BigDecimal, BMWtchctr_fac_FadeTExGas_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchctr_fac_FilRatPTrb_T: CurveType[BigDecimal, BigDecimal], BMWtchctr_fac_TMdlCylOff_T: CurveType[BigDecimal, BigDecimal], BMWtchctr_fac_TrbEffIvs_T: CurveType[BigDecimal, BigDecimal], BMWtchctr_fac_TrbEffPlsCrtn_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchctr_fac_TrbExp_T: CurveType[BigDecimal, BigDecimal], BMWtchctr_fac_gra_FadeTExGas_C: BigDecimal, BMWtchctr_pct_WgBasc_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchctr_swi_ClcPwrWgPosn_C: String, BMWtchctr_t_EgCrtn_T: CurveType[BigDecimal, BigDecimal], BMWtchctr_t_ExGasFilMin_C: BigDecimal, BMWtchctr_t_ExGasMdlScav_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchctr_t_ExGasMdl_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchctr_t_MdlOfsIgRtd_T: CurveType[BigDecimal, BigDecimal], BMWausy_p_DifCat_T: CurveType[BigDecimal, BigDecimal], BMWausy_p_DifEgFlpClsd_T: CurveType[BigDecimal, BigDecimal], BMWausy_p_DifEgFlpOp_T: CurveType[BigDecimal, BigDecimal], BMWausy_p_DifPclFil_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWausy_swi_p_DifCatTot_C: String, KF_AUSY_TURB: MapType[BigDecimal, BigDecimal, BigDecimal], KL_FPKATREL_PUMG: CurveType[BigDecimal, BigDecimal], KL_TABG2TABG_WZINV: CurveType[BigDecimal, BigDecimal], K_ANZAHL_TURBOS: BigDecimal, BMWtchctr_pct_WgBasc_uw: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchCtr_Pwr2Pos_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchctr_cpp_ExGas_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_cpp_ExGas_T")
val BMWtchctr_cw_FadeFlowSpSpt_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_cw_FadeFlowSpSpt_C")
val BMWtchctr_eff_CylOffMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_eff_CylOffMin_C")
val BMWtchctr_fac_FadeFlowSpAcv_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_FadeFlowSpAcv_M")
val BMWtchctr_fac_FadeFlowSpSpt_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_FadeFlowSpSpt_M")
val BMWtchctr_fac_FadeTExGasGc_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_FadeTExGasGc_C")
val BMWtchctr_fac_FadeTExGas_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_FadeTExGas_C")
val BMWtchctr_fac_FadeTExGas_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_FadeTExGas_M")
val BMWtchctr_fac_FilRatPTrb_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_FilRatPTrb_T")
val BMWtchctr_fac_TMdlCylOff_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_TMdlCylOff_T")
val BMWtchctr_fac_TrbEffIvs_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_TrbEffIvs_T")
val BMWtchctr_fac_TrbEffPlsCrtn_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_TrbEffPlsCrtn_M")
val BMWtchctr_fac_TrbExp_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_TrbExp_T")
val BMWtchctr_fac_gra_FadeTExGas_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_gra_FadeTExGas_C")
val BMWtchctr_pct_WgBasc_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_pct_WgBasc_M")
val BMWtchctr_swi_ClcPwrWgPosn_C: String = a2lBin.readCharacteristicWithCast("BMWtchctr_swi_ClcPwrWgPosn_C")
val BMWtchctr_t_EgCrtn_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_t_EgCrtn_T")
val BMWtchctr_t_ExGasFilMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_t_ExGasFilMin_C")
val BMWtchctr_t_ExGasMdlScav_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_t_ExGasMdlScav_M")
val BMWtchctr_t_ExGasMdl_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_t_ExGasMdl_M")
val BMWtchctr_t_MdlOfsIgRtd_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_t_MdlOfsIgRtd_T")
val BMWausy_p_DifCat_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWausy_p_DifCat_T")
val BMWausy_p_DifEgFlpClsd_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWausy_p_DifEgFlpClsd_T")
val BMWausy_p_DifEgFlpOp_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWausy_p_DifEgFlpOp_T")
val BMWausy_p_DifPclFil_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWausy_p_DifPclFil_M")
val BMWausy_swi_p_DifCatTot_C: String = a2lBin.readCharacteristicWithCast("BMWausy_swi_p_DifCatTot_C")
val KF_AUSY_TURB: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_AUSY_TURB")
val KL_FPKATREL_PUMG: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_FPKATREL_PUMG")
val KL_TABG2TABG_WZINV: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_TABG2TABG_WZINV")
val K_ANZAHL_TURBOS: BigDecimal = a2lBin.readCharacteristicWithCast("K_ANZAHL_TURBOS")
val BMWbdy_b_EgFlp_bo: InMeasurement = a2lBin.measurement("BMWbdy_b_EgFlp_bo")
val BMWext_t_GasP2PclFilXab_sw: InMeasurement = a2lBin.measurement("BMWext_t_GasP2PclFilXab_sw")
val BMWgpfp_b_PclFil_bo: InMeasurement = a2lBin.measurement("BMWgpfp_b_PclFil_bo")
val BMWgpfp_fac_AdpPclFil_uw: InMeasurement = a2lBin.measurement("BMWgpfp_fac_AdpPclFil_uw")
val BMWgpfp_m_SootLoad_sw: InMeasurement = a2lBin.measurement("BMWgpfp_m_SootLoad_sw")
val BMWign_ag_OutMv_sw: InMeasurement = a2lBin.measurement("BMWign_ag_OutMv_sw")
val BMWtchbas_b_FlGc_bo: InMeasurement = a2lBin.measurement("BMWtchbas_b_FlGc_bo")
val BMWtchbas_p_Dif_sw: InMeasurement = a2lBin.measurement("BMWtchbas_p_Dif_sw")
val BMWtchco_b_Clc10MilliSec_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Clc10MilliSec_bo")
val BMWtchco_b_ClcCtlr_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_ClcCtlr_bo")
val BMWtchco_st_Opm_ub: InMeasurement = a2lBin.measurement("BMWtchco_st_Opm_ub")
val BMWtchctr_pct_WgAltiCrtn_sw: InMeasurement = a2lBin.measurement("BMWtchctr_pct_WgAltiCrtn_sw")
val BMWtchctr_pct_WgRaw_uw: InMeasurement = a2lBin.measurement("BMWtchctr_pct_WgRaw_uw")
val BMWtchctr_pwr_Trb_uw: InMeasurement = a2lBin.measurement("BMWtchctr_pwr_Trb_uw")
val BMWtchscv_fac__ub: InMeasurement = a2lBin.measurement("BMWtchscv_fac__ub")
val BMWtchsp_mf_Ex_uw: InMeasurement = a2lBin.measurement("BMWtchsp_mf_Ex_uw")
val BMWtchsp_p_ReqDyn_sw: InMeasurement = a2lBin.measurement("BMWtchsp_p_ReqDyn_sw")
val Eta_zyl_aus: InMeasurement = a2lBin.measurement("Eta_zyl_aus")
val Msakzu: InMeasurement = a2lBin.measurement("Msakzu")
val Nkw: InMeasurement = a2lBin.measurement("Nkw")
val Pumg: InMeasurement = a2lBin.measurement("Pumg")
val Rf: InMeasurement = a2lBin.measurement("Rf")
val St_getrdaten: InMeasurement = a2lBin.measurement("St_getrdaten")
val St_getrdaten_B_gangwechsel_gs: InMeasurement = a2lBin.measurement("St_getrdaten.B_gangwechsel_gs")
val Tabg_mw: InMeasurement = a2lBin.measurement("Tabg_mw")
val Vsa_spri_f: InMeasurement = a2lBin.measurement("Vsa_spri_f")
val Zwstat: InMeasurement = a2lBin.measurement("Zwstat")
val BMWtchctr_pct_WgBasc_uw: OutMeasurement = a2lBin.measurement("BMWtchctr_pct_WgBasc_uw")

  BMW_MOD_TchCtr_Pwr2Pos_10ms(BMWbdy_b_EgFlp_bo, BMWext_t_GasP2PclFilXab_sw, BMWgpfp_b_PclFil_bo, BMWgpfp_fac_AdpPclFil_uw, BMWgpfp_m_SootLoad_sw, BMWign_ag_OutMv_sw, BMWtchbas_b_FlGc_bo, BMWtchbas_p_Dif_sw, BMWtchco_b_Clc10MilliSec_bo, BMWtchco_b_ClcCtlr_bo, BMWtchco_st_Opm_ub, BMWtchctr_pct_WgAltiCrtn_sw, BMWtchctr_pct_WgRaw_uw, BMWtchctr_pwr_Trb_uw, BMWtchscv_fac__ub, BMWtchsp_mf_Ex_uw, BMWtchsp_p_ReqDyn_sw, Eta_zyl_aus, Msakzu, Nkw, Pumg, Rf, St_getrdaten, St_getrdaten_B_gangwechsel_gs, Tabg_mw, Vsa_spri_f, Zwstat, BMWtchctr_cpp_ExGas_T, BMWtchctr_cw_FadeFlowSpSpt_C, BMWtchctr_eff_CylOffMin_C, BMWtchctr_fac_FadeFlowSpAcv_M, BMWtchctr_fac_FadeFlowSpSpt_M, BMWtchctr_fac_FadeTExGasGc_C, BMWtchctr_fac_FadeTExGas_C, BMWtchctr_fac_FadeTExGas_M, BMWtchctr_fac_FilRatPTrb_T, BMWtchctr_fac_TMdlCylOff_T, BMWtchctr_fac_TrbEffIvs_T, BMWtchctr_fac_TrbEffPlsCrtn_M, BMWtchctr_fac_TrbExp_T, BMWtchctr_fac_gra_FadeTExGas_C, BMWtchctr_pct_WgBasc_M, BMWtchctr_swi_ClcPwrWgPosn_C, BMWtchctr_t_EgCrtn_T, BMWtchctr_t_ExGasFilMin_C, BMWtchctr_t_ExGasMdlScav_M, BMWtchctr_t_ExGasMdl_M, BMWtchctr_t_MdlOfsIgRtd_T, BMWausy_p_DifCat_T, BMWausy_p_DifEgFlpClsd_T, BMWausy_p_DifEgFlpOp_T, BMWausy_p_DifPclFil_M, BMWausy_swi_p_DifCatTot_C, KF_AUSY_TURB, KL_FPKATREL_PUMG, KL_TABG2TABG_WZINV, K_ANZAHL_TURBOS, BMWtchctr_pct_WgBasc_uw)
}


def BMW_MOD_TchCtr_PwrFade_10ms(BMWtchbas_b_FlGcHld_bo: InMeasurement, BMWtchco_b_AcvPl_bo: InMeasurement, BMWtchco_b_Acv_bo: InMeasurement, BMWtchco_b_Clc10MilliSec_bo: InMeasurement, BMWtchco_b_ClcCtlr_bo: InMeasurement, BMWtchctr_pwr_Ctr_uw: InMeasurement, BMWtchctr_pwr_Pctl_uw: InMeasurement, BMWtchdiag_b_LimPctl_bo: InMeasurement, BMWtchsp_mf_CmprNorm_uw: InMeasurement, BMWtchsp_p_ReqDyn_sw: InMeasurement, BMWtchsp_rat_p_CmprLim_uw: InMeasurement, BMWtchctr_fac_FilLimPctl_C: BigDecimal, BMWtchctr_fac_pwr_CompLimPctl_T: CurveType[BigDecimal, BigDecimal], BMWtchctr_p_ReqDynLimPctl_C: BigDecimal, BMWtchctr_rat_p_ComprLimPctl_T: CurveType[BigDecimal, BigDecimal], BMWtchco_swi_FlGcCtlrOn_C: String, BMWtchctr_pwr_Trb_uw: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchCtr_PwrFade_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchctr_fac_FilLimPctl_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_FilLimPctl_C")
val BMWtchctr_fac_pwr_CompLimPctl_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_pwr_CompLimPctl_T")
val BMWtchctr_p_ReqDynLimPctl_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_p_ReqDynLimPctl_C")
val BMWtchctr_rat_p_ComprLimPctl_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_rat_p_ComprLimPctl_T")
val BMWtchco_swi_FlGcCtlrOn_C: String = a2lBin.readCharacteristicWithCast("BMWtchco_swi_FlGcCtlrOn_C")
val BMWtchbas_b_FlGcHld_bo: InMeasurement = a2lBin.measurement("BMWtchbas_b_FlGcHld_bo")
val BMWtchco_b_AcvPl_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_AcvPl_bo")
val BMWtchco_b_Acv_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Acv_bo")
val BMWtchco_b_Clc10MilliSec_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Clc10MilliSec_bo")
val BMWtchco_b_ClcCtlr_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_ClcCtlr_bo")
val BMWtchctr_pwr_Ctr_uw: InMeasurement = a2lBin.measurement("BMWtchctr_pwr_Ctr_uw")
val BMWtchctr_pwr_Pctl_uw: InMeasurement = a2lBin.measurement("BMWtchctr_pwr_Pctl_uw")
val BMWtchdiag_b_LimPctl_bo: InMeasurement = a2lBin.measurement("BMWtchdiag_b_LimPctl_bo")
val BMWtchsp_mf_CmprNorm_uw: InMeasurement = a2lBin.measurement("BMWtchsp_mf_CmprNorm_uw")
val BMWtchsp_p_ReqDyn_sw: InMeasurement = a2lBin.measurement("BMWtchsp_p_ReqDyn_sw")
val BMWtchsp_rat_p_CmprLim_uw: InMeasurement = a2lBin.measurement("BMWtchsp_rat_p_CmprLim_uw")
val BMWtchctr_pwr_Trb_uw: OutMeasurement = a2lBin.measurement("BMWtchctr_pwr_Trb_uw")

  BMW_MOD_TchCtr_PwrFade_10ms(BMWtchbas_b_FlGcHld_bo, BMWtchco_b_AcvPl_bo, BMWtchco_b_Acv_bo, BMWtchco_b_Clc10MilliSec_bo, BMWtchco_b_ClcCtlr_bo, BMWtchctr_pwr_Ctr_uw, BMWtchctr_pwr_Pctl_uw, BMWtchdiag_b_LimPctl_bo, BMWtchsp_mf_CmprNorm_uw, BMWtchsp_p_ReqDyn_sw, BMWtchsp_rat_p_CmprLim_uw, BMWtchctr_fac_FilLimPctl_C, BMWtchctr_fac_pwr_CompLimPctl_T, BMWtchctr_p_ReqDynLimPctl_C, BMWtchctr_rat_p_ComprLimPctl_T, BMWtchco_swi_FlGcCtlrOn_C, BMWtchctr_pwr_Trb_uw)
}


def BMW_MOD_TchCtr_Pwr_10ms(BMWewgacp_pct_WgAvg_uw: InMeasurement, BMWtchad_fac_Raw_uw: InMeasurement, BMWtchbas_n_Dyn_sw: InMeasurement, BMWtchbas_p_BefCmpr_uw: InMeasurement, BMWtchbas_p_DifGra_sw: InMeasurement, BMWtchbas_p_Dif_sw: InMeasurement, BMWtchco_b_Acv_bo: InMeasurement, BMWtchco_b_Clc10MilliSec_bo: InMeasurement, BMWtchco_b_ClcCtlr_bo: InMeasurement, BMWtchco_st_Opm_ub: InMeasurement, BMWtchctr_pct_WgBefLim_uw: InMeasurement, BMWtchctr_pct_WgLimMax_uw: InMeasurement, BMWtchctr_pct_Wg_uw: InMeasurement, BMWtchsp_fac_mf_CmprNorm_uw: InMeasurement, BMWtchsp_mf_CmprNorm_uw: InMeasurement, BMWtchsp_mf_Ex_uw: InMeasurement, BMWtchsp_p_ReqDyn_sw: InMeasurement, BMWtchsp_rat_p_CmprLim_uw: InMeasurement, BMWtchtbc_fac_t_BefComp_uw: InMeasurement, Gangi_stat: InMeasurement, Nkw: InMeasurement, Var_at: InMeasurement, Var_dkg: InMeasurement, BMWtchco_cw_FadeDynSpt_C: BigDecimal, BMWtchctr_fac_DpDyn_T: CurveType[BigDecimal, BigDecimal], BMWtchctr_fac_Dp_T: CurveType[BigDecimal, BigDecimal], BMWtchctr_fac_FadeDynSpt_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchctr_fac_FadeDyn_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchctr_fac_FilFadeDyn_C: BigDecimal, BMWtchctr_fac_IpMax_C: BigDecimal, BMWtchctr_fac_IpMin_C: BigDecimal, BMWtchctr_fac_IpRstMax_C: BigDecimal, BMWtchctr_fac_IpRstMin_C: BigDecimal, BMWtchctr_fac_Ip_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchctr_n_DynIpRstMax2_C: BigDecimal, BMWtchctr_n_DynIpRstMin2_C: BigDecimal, BMWtchctr_n_PpLimMax_C: BigDecimal, BMWtchctr_n_PpLimMin_C: BigDecimal, BMWtchctr_p_DifCrtnPpDyn_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchctr_p_DifCrtnPp_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchctr_p_DifGraPpLimOff_C: BigDecimal, BMWtchctr_p_DifIpRstMax2_C: BigDecimal, BMWtchctr_p_DifIpRstMin2_C: BigDecimal, BMWtchctr_p_DifPpLimOff_C: BigDecimal, BMWtchctr_p_DynIpRstMax2_C: BigDecimal, BMWtchctr_p_DynIpRstMin2_C: BigDecimal, BMWtchctr_pct_WgPosnIpStopMax2_C: BigDecimal, BMWtchctr_pct_WgPosnIpStopMin2_C: BigDecimal, BMWtchctr_pct_WgSpIpStopMax2_C: BigDecimal, BMWtchctr_pwr_CmprGra_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchctr_pwr_Dp_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchctr_pwr_Pctl_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchctr_rat_p_CmprPpLimMax_C: BigDecimal, BMWtchctr_rat_p_CmprPpLimMin_C: BigDecimal, BMWtchctr_swi_IpPwrAcvn_C: String, BMWtchctr_swi_IpRstMan2_C: String, BMWtchctr_ti_DlyPpLimOff_C: BigDecimal, BMWtchctr_ti_ToutPpLim_C: BigDecimal, BMWtchctr_b_IpRst2_bo: OutMeasurement, BMWtchctr_b_IpStop2_bo: OutMeasurement, BMWtchctr_fac_Ip_uw: OutMeasurement, BMWtchctr_pct_WgAct_uw: OutMeasurement, BMWtchctr_pwr_Ctr_uw: OutMeasurement, BMWtchctr_pwr_Pctl_uw: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchCtr_Pwr_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchco_cw_FadeDynSpt_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchco_cw_FadeDynSpt_C")
val BMWtchctr_fac_DpDyn_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_DpDyn_T")
val BMWtchctr_fac_Dp_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_Dp_T")
val BMWtchctr_fac_FadeDynSpt_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_FadeDynSpt_M")
val BMWtchctr_fac_FadeDyn_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_FadeDyn_M")
val BMWtchctr_fac_FilFadeDyn_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_FilFadeDyn_C")
val BMWtchctr_fac_IpMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_IpMax_C")
val BMWtchctr_fac_IpMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_IpMin_C")
val BMWtchctr_fac_IpRstMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_IpRstMax_C")
val BMWtchctr_fac_IpRstMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_IpRstMin_C")
val BMWtchctr_fac_Ip_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_fac_Ip_M")
val BMWtchctr_n_DynIpRstMax2_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_n_DynIpRstMax2_C")
val BMWtchctr_n_DynIpRstMin2_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_n_DynIpRstMin2_C")
val BMWtchctr_n_PpLimMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_n_PpLimMax_C")
val BMWtchctr_n_PpLimMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_n_PpLimMin_C")
val BMWtchctr_p_DifCrtnPpDyn_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_p_DifCrtnPpDyn_M")
val BMWtchctr_p_DifCrtnPp_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_p_DifCrtnPp_M")
val BMWtchctr_p_DifGraPpLimOff_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_p_DifGraPpLimOff_C")
val BMWtchctr_p_DifIpRstMax2_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_p_DifIpRstMax2_C")
val BMWtchctr_p_DifIpRstMin2_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_p_DifIpRstMin2_C")
val BMWtchctr_p_DifPpLimOff_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_p_DifPpLimOff_C")
val BMWtchctr_p_DynIpRstMax2_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_p_DynIpRstMax2_C")
val BMWtchctr_p_DynIpRstMin2_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_p_DynIpRstMin2_C")
val BMWtchctr_pct_WgPosnIpStopMax2_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_pct_WgPosnIpStopMax2_C")
val BMWtchctr_pct_WgPosnIpStopMin2_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_pct_WgPosnIpStopMin2_C")
val BMWtchctr_pct_WgSpIpStopMax2_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_pct_WgSpIpStopMax2_C")
val BMWtchctr_pwr_CmprGra_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_pwr_CmprGra_M")
val BMWtchctr_pwr_Dp_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_pwr_Dp_M")
val BMWtchctr_pwr_Pctl_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchctr_pwr_Pctl_M")
val BMWtchctr_rat_p_CmprPpLimMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_rat_p_CmprPpLimMax_C")
val BMWtchctr_rat_p_CmprPpLimMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_rat_p_CmprPpLimMin_C")
val BMWtchctr_swi_IpPwrAcvn_C: String = a2lBin.readCharacteristicWithCast("BMWtchctr_swi_IpPwrAcvn_C")
val BMWtchctr_swi_IpRstMan2_C: String = a2lBin.readCharacteristicWithCast("BMWtchctr_swi_IpRstMan2_C")
val BMWtchctr_ti_DlyPpLimOff_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_ti_DlyPpLimOff_C")
val BMWtchctr_ti_ToutPpLim_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchctr_ti_ToutPpLim_C")
val BMWewgacp_pct_WgAvg_uw: InMeasurement = a2lBin.measurement("BMWewgacp_pct_WgAvg_uw")
val BMWtchad_fac_Raw_uw: InMeasurement = a2lBin.measurement("BMWtchad_fac_Raw_uw")
val BMWtchbas_n_Dyn_sw: InMeasurement = a2lBin.measurement("BMWtchbas_n_Dyn_sw")
val BMWtchbas_p_BefCmpr_uw: InMeasurement = a2lBin.measurement("BMWtchbas_p_BefCmpr_uw")
val BMWtchbas_p_DifGra_sw: InMeasurement = a2lBin.measurement("BMWtchbas_p_DifGra_sw")
val BMWtchbas_p_Dif_sw: InMeasurement = a2lBin.measurement("BMWtchbas_p_Dif_sw")
val BMWtchco_b_Acv_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Acv_bo")
val BMWtchco_b_Clc10MilliSec_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Clc10MilliSec_bo")
val BMWtchco_b_ClcCtlr_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_ClcCtlr_bo")
val BMWtchco_st_Opm_ub: InMeasurement = a2lBin.measurement("BMWtchco_st_Opm_ub")
val BMWtchctr_pct_WgBefLim_uw: InMeasurement = a2lBin.measurement("BMWtchctr_pct_WgBefLim_uw")
val BMWtchctr_pct_WgLimMax_uw: InMeasurement = a2lBin.measurement("BMWtchctr_pct_WgLimMax_uw")
val BMWtchctr_pct_Wg_uw: InMeasurement = a2lBin.measurement("BMWtchctr_pct_Wg_uw")
val BMWtchsp_fac_mf_CmprNorm_uw: InMeasurement = a2lBin.measurement("BMWtchsp_fac_mf_CmprNorm_uw")
val BMWtchsp_mf_CmprNorm_uw: InMeasurement = a2lBin.measurement("BMWtchsp_mf_CmprNorm_uw")
val BMWtchsp_mf_Ex_uw: InMeasurement = a2lBin.measurement("BMWtchsp_mf_Ex_uw")
val BMWtchsp_p_ReqDyn_sw: InMeasurement = a2lBin.measurement("BMWtchsp_p_ReqDyn_sw")
val BMWtchsp_rat_p_CmprLim_uw: InMeasurement = a2lBin.measurement("BMWtchsp_rat_p_CmprLim_uw")
val BMWtchtbc_fac_t_BefComp_uw: InMeasurement = a2lBin.measurement("BMWtchtbc_fac_t_BefComp_uw")
val Gangi_stat: InMeasurement = a2lBin.measurement("Gangi_stat")
val Nkw: InMeasurement = a2lBin.measurement("Nkw")
val Var_at: InMeasurement = a2lBin.measurement("Var_at")
val Var_dkg: InMeasurement = a2lBin.measurement("Var_dkg")
val BMWtchctr_b_IpRst2_bo: OutMeasurement = a2lBin.measurement("BMWtchctr_b_IpRst2_bo")
val BMWtchctr_b_IpStop2_bo: OutMeasurement = a2lBin.measurement("BMWtchctr_b_IpStop2_bo")
val BMWtchctr_fac_Ip_uw: OutMeasurement = a2lBin.measurement("BMWtchctr_fac_Ip_uw")
val BMWtchctr_pct_WgAct_uw: OutMeasurement = a2lBin.measurement("BMWtchctr_pct_WgAct_uw")
val BMWtchctr_pwr_Ctr_uw: OutMeasurement = a2lBin.measurement("BMWtchctr_pwr_Ctr_uw")
val BMWtchctr_pwr_Pctl_uw: OutMeasurement = a2lBin.measurement("BMWtchctr_pwr_Pctl_uw")

  BMW_MOD_TchCtr_Pwr_10ms(BMWewgacp_pct_WgAvg_uw, BMWtchad_fac_Raw_uw, BMWtchbas_n_Dyn_sw, BMWtchbas_p_BefCmpr_uw, BMWtchbas_p_DifGra_sw, BMWtchbas_p_Dif_sw, BMWtchco_b_Acv_bo, BMWtchco_b_Clc10MilliSec_bo, BMWtchco_b_ClcCtlr_bo, BMWtchco_st_Opm_ub, BMWtchctr_pct_WgBefLim_uw, BMWtchctr_pct_WgLimMax_uw, BMWtchctr_pct_Wg_uw, BMWtchsp_fac_mf_CmprNorm_uw, BMWtchsp_mf_CmprNorm_uw, BMWtchsp_mf_Ex_uw, BMWtchsp_p_ReqDyn_sw, BMWtchsp_rat_p_CmprLim_uw, BMWtchtbc_fac_t_BefComp_uw, Gangi_stat, Nkw, Var_at, Var_dkg, BMWtchco_cw_FadeDynSpt_C, BMWtchctr_fac_DpDyn_T, BMWtchctr_fac_Dp_T, BMWtchctr_fac_FadeDynSpt_M, BMWtchctr_fac_FadeDyn_M, BMWtchctr_fac_FilFadeDyn_C, BMWtchctr_fac_IpMax_C, BMWtchctr_fac_IpMin_C, BMWtchctr_fac_IpRstMax_C, BMWtchctr_fac_IpRstMin_C, BMWtchctr_fac_Ip_M, BMWtchctr_n_DynIpRstMax2_C, BMWtchctr_n_DynIpRstMin2_C, BMWtchctr_n_PpLimMax_C, BMWtchctr_n_PpLimMin_C, BMWtchctr_p_DifCrtnPpDyn_M, BMWtchctr_p_DifCrtnPp_M, BMWtchctr_p_DifGraPpLimOff_C, BMWtchctr_p_DifIpRstMax2_C, BMWtchctr_p_DifIpRstMin2_C, BMWtchctr_p_DifPpLimOff_C, BMWtchctr_p_DynIpRstMax2_C, BMWtchctr_p_DynIpRstMin2_C, BMWtchctr_pct_WgPosnIpStopMax2_C, BMWtchctr_pct_WgPosnIpStopMin2_C, BMWtchctr_pct_WgSpIpStopMax2_C, BMWtchctr_pwr_CmprGra_M, BMWtchctr_pwr_Dp_M, BMWtchctr_pwr_Pctl_M, BMWtchctr_rat_p_CmprPpLimMax_C, BMWtchctr_rat_p_CmprPpLimMin_C, BMWtchctr_swi_IpPwrAcvn_C, BMWtchctr_swi_IpRstMan2_C, BMWtchctr_ti_DlyPpLimOff_C, BMWtchctr_ti_ToutPpLim_C, BMWtchctr_b_IpRst2_bo, BMWtchctr_b_IpStop2_bo, BMWtchctr_fac_Ip_uw, BMWtchctr_pct_WgAct_uw, BMWtchctr_pwr_Ctr_uw, BMWtchctr_pwr_Pctl_uw)
}


def BMW_MOD_TchDiag_10ms(BMWeisy_p_PreThrPlau_uw: InMeasurement, BMWewgacp_posn__sw: InMeasurement, BMWewgsp_posn__sw: InMeasurement, Nkw: InMeasurement, Pssol: InMeasurement, Rf_soll: InMeasurement, BMWtchdiag_fac_FilPPreThr_C: BigDecimal, BMWtchdiag_p_MnfSp_C: BigDecimal, BMWtchdiag_p_PreThrDynMin_T: CurveType[BigDecimal, BigDecimal], BMWtchdiag_p_PreThrPlau_C: BigDecimal, BMWtchdiag_p_PreThrPlau_T: CurveType[BigDecimal, BigDecimal], BMWtchdiag_posn_Dif10Milli_C: BigDecimal, BMWtchdiag_posn_DifThd_C: BigDecimal, BMWtchdiag_posn_WgMax_C: BigDecimal, BMWtchdiag_rf_MaxPBefThrHiInc_C: BigDecimal, BMWtchdiag_rf_MaxPBefThrHi_T: CurveType[BigDecimal, BigDecimal], BMWtchdiag_rf_Min_C: BigDecimal, BMWtchdiag_ti_DlyPBefThrHi2_C: BigDecimal, BMWtchdiag_ti_DlyPBefThrHi_C: BigDecimal, BMWtchdiag_b_PBefThrHi_bo: OutMeasurement, BMWtchdiag_rf_MaxPBefThrHi_sw: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchDiag_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchdiag_fac_FilPPreThr_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchdiag_fac_FilPPreThr_C")
val BMWtchdiag_p_MnfSp_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchdiag_p_MnfSp_C")
val BMWtchdiag_p_PreThrDynMin_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchdiag_p_PreThrDynMin_T")
val BMWtchdiag_p_PreThrPlau_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchdiag_p_PreThrPlau_C")
val BMWtchdiag_p_PreThrPlau_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchdiag_p_PreThrPlau_T")
val BMWtchdiag_posn_Dif10Milli_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchdiag_posn_Dif10Milli_C")
val BMWtchdiag_posn_DifThd_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchdiag_posn_DifThd_C")
val BMWtchdiag_posn_WgMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchdiag_posn_WgMax_C")
val BMWtchdiag_rf_MaxPBefThrHiInc_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchdiag_rf_MaxPBefThrHiInc_C")
val BMWtchdiag_rf_MaxPBefThrHi_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchdiag_rf_MaxPBefThrHi_T")
val BMWtchdiag_rf_Min_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchdiag_rf_Min_C")
val BMWtchdiag_ti_DlyPBefThrHi2_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchdiag_ti_DlyPBefThrHi2_C")
val BMWtchdiag_ti_DlyPBefThrHi_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchdiag_ti_DlyPBefThrHi_C")
val BMWeisy_p_PreThrPlau_uw: InMeasurement = a2lBin.measurement("BMWeisy_p_PreThrPlau_uw")
val BMWewgacp_posn__sw: InMeasurement = a2lBin.measurement("BMWewgacp_posn__sw")
val BMWewgsp_posn__sw: InMeasurement = a2lBin.measurement("BMWewgsp_posn__sw")
val Nkw: InMeasurement = a2lBin.measurement("Nkw")
val Pssol: InMeasurement = a2lBin.measurement("Pssol")
val Rf_soll: InMeasurement = a2lBin.measurement("Rf_soll")
val BMWtchdiag_b_PBefThrHi_bo: OutMeasurement = a2lBin.measurement("BMWtchdiag_b_PBefThrHi_bo")
val BMWtchdiag_rf_MaxPBefThrHi_sw: OutMeasurement = a2lBin.measurement("BMWtchdiag_rf_MaxPBefThrHi_sw")

  BMW_MOD_TchDiag_10ms(BMWeisy_p_PreThrPlau_uw, BMWewgacp_posn__sw, BMWewgsp_posn__sw, Nkw, Pssol, Rf_soll, BMWtchdiag_fac_FilPPreThr_C, BMWtchdiag_p_MnfSp_C, BMWtchdiag_p_PreThrDynMin_T, BMWtchdiag_p_PreThrPlau_C, BMWtchdiag_p_PreThrPlau_T, BMWtchdiag_posn_Dif10Milli_C, BMWtchdiag_posn_DifThd_C, BMWtchdiag_posn_WgMax_C, BMWtchdiag_rf_MaxPBefThrHiInc_C, BMWtchdiag_rf_MaxPBefThrHi_T, BMWtchdiag_rf_Min_C, BMWtchdiag_ti_DlyPBefThrHi2_C, BMWtchdiag_ti_DlyPBefThrHi_C, BMWtchdiag_b_PBefThrHi_bo, BMWtchdiag_rf_MaxPBefThrHi_sw)
}


def BMW_MOD_TchDiag_Co_100ms(BMWeisy_b_PSnsrDblErr_bo: InMeasurement, BMWeisy_b_PSnsrPreThrErr_bo: InMeasurement, BMWemm_ct_StDrv_ub: InMeasurement, BMWeng_b_StrEnd_bo: InMeasurement, BMWtchdiag_swi_FidNew_C: String, BMWtchdiag_swi_Rst_C: String, BMWtchdiag_b_AdpOff_bo: OutMeasurement, BMWtchdiag_b_CtlrOff_bo: OutMeasurement, BMWtchdiag_b_DiagOff_bo: OutMeasurement, BMWtchdiag_b_LimPctl_bo: OutMeasurement, BMWtchdiag_b_rf_Lim_bo: OutMeasurement, BMWtchdiag_b_tq_Lim_bo: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchDiag_Co_100ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchdiag_swi_FidNew_C: String = a2lBin.readCharacteristicWithCast("BMWtchdiag_swi_FidNew_C")
val BMWtchdiag_swi_Rst_C: String = a2lBin.readCharacteristicWithCast("BMWtchdiag_swi_Rst_C")
val BMWeisy_b_PSnsrDblErr_bo: InMeasurement = a2lBin.measurement("BMWeisy_b_PSnsrDblErr_bo")
val BMWeisy_b_PSnsrPreThrErr_bo: InMeasurement = a2lBin.measurement("BMWeisy_b_PSnsrPreThrErr_bo")
val BMWemm_ct_StDrv_ub: InMeasurement = a2lBin.measurement("BMWemm_ct_StDrv_ub")
val BMWeng_b_StrEnd_bo: InMeasurement = a2lBin.measurement("BMWeng_b_StrEnd_bo")
val BMWtchdiag_b_AdpOff_bo: OutMeasurement = a2lBin.measurement("BMWtchdiag_b_AdpOff_bo")
val BMWtchdiag_b_CtlrOff_bo: OutMeasurement = a2lBin.measurement("BMWtchdiag_b_CtlrOff_bo")
val BMWtchdiag_b_DiagOff_bo: OutMeasurement = a2lBin.measurement("BMWtchdiag_b_DiagOff_bo")
val BMWtchdiag_b_LimPctl_bo: OutMeasurement = a2lBin.measurement("BMWtchdiag_b_LimPctl_bo")
val BMWtchdiag_b_rf_Lim_bo: OutMeasurement = a2lBin.measurement("BMWtchdiag_b_rf_Lim_bo")
val BMWtchdiag_b_tq_Lim_bo: OutMeasurement = a2lBin.measurement("BMWtchdiag_b_tq_Lim_bo")

  BMW_MOD_TchDiag_Co_100ms(BMWeisy_b_PSnsrDblErr_bo, BMWeisy_b_PSnsrPreThrErr_bo, BMWemm_ct_StDrv_ub, BMWeng_b_StrEnd_bo, BMWtchdiag_swi_FidNew_C, BMWtchdiag_swi_Rst_C, BMWtchdiag_b_AdpOff_bo, BMWtchdiag_b_CtlrOff_bo, BMWtchdiag_b_DiagOff_bo, BMWtchdiag_b_LimPctl_bo, BMWtchdiag_b_rf_Lim_bo, BMWtchdiag_b_tq_Lim_bo)
}


def BMW_MOD_TchDiag_Obd_100ms(BMWeisy_p_PreThrPlau_uw: InMeasurement, BMWtchco_b_Acv_bo: InMeasurement, BMWtchdiag_b_Acv_bo: InMeasurement, Pumg: InMeasurement, BMWtchdiag_ti_DlyDiagCmpl_C: BigDecimal, BMWtchdiag_b_RbmDen_bo: OutMeasurement, BMWtchdiag_b_RbmNum_bo: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchDiag_Obd_100ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchdiag_ti_DlyDiagCmpl_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchdiag_ti_DlyDiagCmpl_C")
val BMWeisy_p_PreThrPlau_uw: InMeasurement = a2lBin.measurement("BMWeisy_p_PreThrPlau_uw")
val BMWtchco_b_Acv_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Acv_bo")
val BMWtchdiag_b_Acv_bo: InMeasurement = a2lBin.measurement("BMWtchdiag_b_Acv_bo")
val Pumg: InMeasurement = a2lBin.measurement("Pumg")
val BMWtchdiag_b_RbmDen_bo: OutMeasurement = a2lBin.measurement("BMWtchdiag_b_RbmDen_bo")
val BMWtchdiag_b_RbmNum_bo: OutMeasurement = a2lBin.measurement("BMWtchdiag_b_RbmNum_bo")

  BMW_MOD_TchDiag_Obd_100ms(BMWeisy_p_PreThrPlau_uw, BMWtchco_b_Acv_bo, BMWtchdiag_b_Acv_bo, Pumg, BMWtchdiag_ti_DlyDiagCmpl_C, BMWtchdiag_b_RbmDen_bo, BMWtchdiag_b_RbmNum_bo)
}


def BMW_MOD_TchDiag_P_100ms(BMWtchbas_b_RaceStr_bo: InMeasurement, BMWtchbas_p_Dif_sw: InMeasurement, BMWtchbas_p_TcBasc_uw: InMeasurement, BMWtchco_b_Acv_bo: InMeasurement, BMWtchctr_pct_WgBefLim_uw: InMeasurement, BMWtchctr_pct_WgCrtd3_uw: InMeasurement, BMWtchctr_pwr_Pctl_uw: InMeasurement, BMWtchctr_pwr_Trb_uw: InMeasurement, BMWtchdiag_b_DiagOff_bo: InMeasurement, BMWtchdiag_b_PBefThrHi_bo: InMeasurement, BMWtchsp_p_Req_uw: InMeasurement, BMWtchsp_rat_p_CmprLim_uw: InMeasurement, BMWtchtbc_t_BefCmprMnfAvg_sw: InMeasurement, Eta_zyl_aus: InMeasurement, Mshfm_kor: InMeasurement, Mszyl_diag: InMeasurement, Nkw: InMeasurement, Pumg: InMeasurement, Rf_soll: InMeasurement, BMWtchdiag_fac_FilRatAirFlow_C: BigDecimal, BMWtchdiag_n_EngMax_T: CurveType[BigDecimal, BigDecimal], BMWtchdiag_n_EngMin_C: BigDecimal, BMWtchdiag_p_OfsDiagOn_T: CurveType[BigDecimal, BigDecimal], BMWtchdiag_pct_WgPHi_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchdiag_pct_WgPLo_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchdiag_rat_mf_NotOkMin_C: BigDecimal, BMWtchdiag_rat_mf_OkMax_C: BigDecimal, BMWtchdiag_rat_mf_OkMin_C: BigDecimal, BMWtchdiag_rat_p_CmprMax_T: CurveType[BigDecimal, BigDecimal], BMWtchdiag_rat_p_CmprMin_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchdiag_rf_SpMin_C: BigDecimal, BMWtchdiag_swi_PBefThrHiFis_C: String, BMWtchdiag_ti_DebPLo_C: BigDecimal, BMWtchdiag_ti_DlyHiLo_T: CurveType[BigDecimal, BigDecimal], BMWtchdiag_b_Acv_bo: OutMeasurement, BMWtchdiag_b_PHi_bo: OutMeasurement, BMWtchdiag_b_PLo_bo: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchDiag_P_100ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchdiag_fac_FilRatAirFlow_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchdiag_fac_FilRatAirFlow_C")
val BMWtchdiag_n_EngMax_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchdiag_n_EngMax_T")
val BMWtchdiag_n_EngMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchdiag_n_EngMin_C")
val BMWtchdiag_p_OfsDiagOn_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchdiag_p_OfsDiagOn_T")
val BMWtchdiag_pct_WgPHi_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchdiag_pct_WgPHi_M")
val BMWtchdiag_pct_WgPLo_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchdiag_pct_WgPLo_M")
val BMWtchdiag_rat_mf_NotOkMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchdiag_rat_mf_NotOkMin_C")
val BMWtchdiag_rat_mf_OkMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchdiag_rat_mf_OkMax_C")
val BMWtchdiag_rat_mf_OkMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchdiag_rat_mf_OkMin_C")
val BMWtchdiag_rat_p_CmprMax_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchdiag_rat_p_CmprMax_T")
val BMWtchdiag_rat_p_CmprMin_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchdiag_rat_p_CmprMin_M")
val BMWtchdiag_rf_SpMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchdiag_rf_SpMin_C")
val BMWtchdiag_swi_PBefThrHiFis_C: String = a2lBin.readCharacteristicWithCast("BMWtchdiag_swi_PBefThrHiFis_C")
val BMWtchdiag_ti_DebPLo_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchdiag_ti_DebPLo_C")
val BMWtchdiag_ti_DlyHiLo_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchdiag_ti_DlyHiLo_T")
val BMWtchbas_b_RaceStr_bo: InMeasurement = a2lBin.measurement("BMWtchbas_b_RaceStr_bo")
val BMWtchbas_p_Dif_sw: InMeasurement = a2lBin.measurement("BMWtchbas_p_Dif_sw")
val BMWtchbas_p_TcBasc_uw: InMeasurement = a2lBin.measurement("BMWtchbas_p_TcBasc_uw")
val BMWtchco_b_Acv_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Acv_bo")
val BMWtchctr_pct_WgBefLim_uw: InMeasurement = a2lBin.measurement("BMWtchctr_pct_WgBefLim_uw")
val BMWtchctr_pct_WgCrtd3_uw: InMeasurement = a2lBin.measurement("BMWtchctr_pct_WgCrtd3_uw")
val BMWtchctr_pwr_Pctl_uw: InMeasurement = a2lBin.measurement("BMWtchctr_pwr_Pctl_uw")
val BMWtchctr_pwr_Trb_uw: InMeasurement = a2lBin.measurement("BMWtchctr_pwr_Trb_uw")
val BMWtchdiag_b_DiagOff_bo: InMeasurement = a2lBin.measurement("BMWtchdiag_b_DiagOff_bo")
val BMWtchdiag_b_PBefThrHi_bo: InMeasurement = a2lBin.measurement("BMWtchdiag_b_PBefThrHi_bo")
val BMWtchsp_p_Req_uw: InMeasurement = a2lBin.measurement("BMWtchsp_p_Req_uw")
val BMWtchsp_rat_p_CmprLim_uw: InMeasurement = a2lBin.measurement("BMWtchsp_rat_p_CmprLim_uw")
val BMWtchtbc_t_BefCmprMnfAvg_sw: InMeasurement = a2lBin.measurement("BMWtchtbc_t_BefCmprMnfAvg_sw")
val Eta_zyl_aus: InMeasurement = a2lBin.measurement("Eta_zyl_aus")
val Mshfm_kor: InMeasurement = a2lBin.measurement("Mshfm_kor")
val Mszyl_diag: InMeasurement = a2lBin.measurement("Mszyl_diag")
val Nkw: InMeasurement = a2lBin.measurement("Nkw")
val Pumg: InMeasurement = a2lBin.measurement("Pumg")
val Rf_soll: InMeasurement = a2lBin.measurement("Rf_soll")
val BMWtchdiag_b_Acv_bo: OutMeasurement = a2lBin.measurement("BMWtchdiag_b_Acv_bo")
val BMWtchdiag_b_PHi_bo: OutMeasurement = a2lBin.measurement("BMWtchdiag_b_PHi_bo")
val BMWtchdiag_b_PLo_bo: OutMeasurement = a2lBin.measurement("BMWtchdiag_b_PLo_bo")

  BMW_MOD_TchDiag_P_100ms(BMWtchbas_b_RaceStr_bo, BMWtchbas_p_Dif_sw, BMWtchbas_p_TcBasc_uw, BMWtchco_b_Acv_bo, BMWtchctr_pct_WgBefLim_uw, BMWtchctr_pct_WgCrtd3_uw, BMWtchctr_pwr_Pctl_uw, BMWtchctr_pwr_Trb_uw, BMWtchdiag_b_DiagOff_bo, BMWtchdiag_b_PBefThrHi_bo, BMWtchsp_p_Req_uw, BMWtchsp_rat_p_CmprLim_uw, BMWtchtbc_t_BefCmprMnfAvg_sw, Eta_zyl_aus, Mshfm_kor, Mszyl_diag, Nkw, Pumg, Rf_soll, BMWtchdiag_fac_FilRatAirFlow_C, BMWtchdiag_n_EngMax_T, BMWtchdiag_n_EngMin_C, BMWtchdiag_p_OfsDiagOn_T, BMWtchdiag_pct_WgPHi_M, BMWtchdiag_pct_WgPLo_M, BMWtchdiag_rat_mf_NotOkMin_C, BMWtchdiag_rat_mf_OkMax_C, BMWtchdiag_rat_mf_OkMin_C, BMWtchdiag_rat_p_CmprMax_T, BMWtchdiag_rat_p_CmprMin_M, BMWtchdiag_rf_SpMin_C, BMWtchdiag_swi_PBefThrHiFis_C, BMWtchdiag_ti_DebPLo_C, BMWtchdiag_ti_DlyHiLo_T, BMWtchdiag_b_Acv_bo, BMWtchdiag_b_PHi_bo, BMWtchdiag_b_PLo_bo)
}


def BMW_MOD_TchOb_Co_10ms(BMWeng_b_StrEnd_bo: InMeasurement, BMWtchbas_p_DifGra_sw: InMeasurement, BMWtchbas_p_Dif_sw: InMeasurement, BMWtchco_b_Acv_bo: InMeasurement, BMWtchco_b_Clc10MilliSec2_bo: InMeasurement, BMWtchob_b_Trig_bo: InMeasurement, BMWtchob_tq_Delt_sw: InMeasurement, Nkw: InMeasurement, BMWtchob_p_DifEnd2_C: BigDecimal, BMWtchob_p_DifEnd_C: BigDecimal, BMWtchob_p_DifGraEnd_C: BigDecimal, BMWtchob_ti_HldChgSp_T: CurveType[BigDecimal, BigDecimal], BMWtchob_ti_Hld_C: BigDecimal, BMWtchob_ti_Max_T: CurveType[BigDecimal, BigDecimal], BMWtchob_b_HldChgSp_bo: OutMeasurement, BMWtchob_st_Opm_ub: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchOb_Co_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchob_p_DifEnd2_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchob_p_DifEnd2_C")
val BMWtchob_p_DifEnd_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchob_p_DifEnd_C")
val BMWtchob_p_DifGraEnd_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchob_p_DifGraEnd_C")
val BMWtchob_ti_HldChgSp_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchob_ti_HldChgSp_T")
val BMWtchob_ti_Hld_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchob_ti_Hld_C")
val BMWtchob_ti_Max_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchob_ti_Max_T")
val BMWeng_b_StrEnd_bo: InMeasurement = a2lBin.measurement("BMWeng_b_StrEnd_bo")
val BMWtchbas_p_DifGra_sw: InMeasurement = a2lBin.measurement("BMWtchbas_p_DifGra_sw")
val BMWtchbas_p_Dif_sw: InMeasurement = a2lBin.measurement("BMWtchbas_p_Dif_sw")
val BMWtchco_b_Acv_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Acv_bo")
val BMWtchco_b_Clc10MilliSec2_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Clc10MilliSec2_bo")
val BMWtchob_b_Trig_bo: InMeasurement = a2lBin.measurement("BMWtchob_b_Trig_bo")
val BMWtchob_tq_Delt_sw: InMeasurement = a2lBin.measurement("BMWtchob_tq_Delt_sw")
val Nkw: InMeasurement = a2lBin.measurement("Nkw")
val BMWtchob_b_HldChgSp_bo: OutMeasurement = a2lBin.measurement("BMWtchob_b_HldChgSp_bo")
val BMWtchob_st_Opm_ub: OutMeasurement = a2lBin.measurement("BMWtchob_st_Opm_ub")

  BMW_MOD_TchOb_Co_10ms(BMWeng_b_StrEnd_bo, BMWtchbas_p_DifGra_sw, BMWtchbas_p_Dif_sw, BMWtchco_b_Acv_bo, BMWtchco_b_Clc10MilliSec2_bo, BMWtchob_b_Trig_bo, BMWtchob_tq_Delt_sw, Nkw, BMWtchob_p_DifEnd2_C, BMWtchob_p_DifEnd_C, BMWtchob_p_DifGraEnd_C, BMWtchob_ti_HldChgSp_T, BMWtchob_ti_Hld_C, BMWtchob_ti_Max_T, BMWtchob_b_HldChgSp_bo, BMWtchob_st_Opm_ub)
}


def BMW_MOD_TchOb_Tq_10ms(BMWtchco_b_Clc10MilliSec2_bo: InMeasurement, BMWtchob_st_Opm_ub: InMeasurement, Gangi_stat: InMeasurement, Nkw: InMeasurement, St_egsprog_kor: InMeasurement, St_progression: InMeasurement, St_progression_B_eco: InMeasurement, St_progression_B_sport: InMeasurement, Tans: InMeasurement, Var_dkg: InMeasurement, Var_hs: InMeasurement, BMWtchob_cw_AtEna_T: CurveType[String, BigDecimal], BMWtchob_cw_DctEna_T: CurveType[String, BigDecimal], BMWtchob_cw_MtEna_C: BigDecimal, BMWtchob_cw_MtSptEna_C: BigDecimal, BMWtchob_fac_tq_Delt_T: CurveType[BigDecimal, BigDecimal], BMWtchob_swi_DeacEco_C: String, BMWtchob_tq_Dec_C: BigDecimal, BMWtchob_tq_Delt_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchob_tq_Delt_sw: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchOb_Tq_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchob_cw_AtEna_T: CurveType[String, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchob_cw_AtEna_T")
val BMWtchob_cw_DctEna_T: CurveType[String, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchob_cw_DctEna_T")
val BMWtchob_cw_MtEna_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchob_cw_MtEna_C")
val BMWtchob_cw_MtSptEna_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchob_cw_MtSptEna_C")
val BMWtchob_fac_tq_Delt_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchob_fac_tq_Delt_T")
val BMWtchob_swi_DeacEco_C: String = a2lBin.readCharacteristicWithCast("BMWtchob_swi_DeacEco_C")
val BMWtchob_tq_Dec_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchob_tq_Dec_C")
val BMWtchob_tq_Delt_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchob_tq_Delt_M")
val BMWtchco_b_Clc10MilliSec2_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Clc10MilliSec2_bo")
val BMWtchob_st_Opm_ub: InMeasurement = a2lBin.measurement("BMWtchob_st_Opm_ub")
val Gangi_stat: InMeasurement = a2lBin.measurement("Gangi_stat")
val Nkw: InMeasurement = a2lBin.measurement("Nkw")
val St_egsprog_kor: InMeasurement = a2lBin.measurement("St_egsprog_kor")
val St_progression: InMeasurement = a2lBin.measurement("St_progression")
val St_progression_B_eco: InMeasurement = a2lBin.measurement("St_progression.B_eco")
val St_progression_B_sport: InMeasurement = a2lBin.measurement("St_progression.B_sport")
val Tans: InMeasurement = a2lBin.measurement("Tans")
val Var_dkg: InMeasurement = a2lBin.measurement("Var_dkg")
val Var_hs: InMeasurement = a2lBin.measurement("Var_hs")
val BMWtchob_tq_Delt_sw: OutMeasurement = a2lBin.measurement("BMWtchob_tq_Delt_sw")

  BMW_MOD_TchOb_Tq_10ms(BMWtchco_b_Clc10MilliSec2_bo, BMWtchob_st_Opm_ub, Gangi_stat, Nkw, St_egsprog_kor, St_progression, St_progression_B_eco, St_progression_B_sport, Tans, Var_dkg, Var_hs, BMWtchob_cw_AtEna_T, BMWtchob_cw_DctEna_T, BMWtchob_cw_MtEna_C, BMWtchob_cw_MtSptEna_C, BMWtchob_fac_tq_Delt_T, BMWtchob_swi_DeacEco_C, BMWtchob_tq_Dec_C, BMWtchob_tq_Delt_M, BMWtchob_tq_Delt_sw)
}


def BMW_MOD_TchOb_Trig_10ms(BMWtchbas_p_DifGra_sw: InMeasurement, BMWtchbas_p_Dif_sw: InMeasurement, BMWtchco_b_Clc10MilliSec2_bo: InMeasurement, BMWtchdiag_b_rf_Lim_bo: InMeasurement, BMWtchdiag_b_tq_Lim_bo: InMeasurement, Gangi_stat: InMeasurement, Mdk_wunsch: InMeasurement, Mdk_wunsch_grad: InMeasurement, Pwg_ist: InMeasurement, St_as_plaus: InMeasurement, St_as_plaus_B_anhang_plaus: InMeasurement, St_as_plaus_B_csn_egs_plaus: InMeasurement, St_as_plaus_B_egs_khl1_plaus: InMeasurement, St_as_plaus_B_egs_khl2_plaus: InMeasurement, St_as_plaus_B_fagurt_plaus: InMeasurement, St_as_plaus_B_fg_overboost_em2_plaus: InMeasurement, St_as_plaus_B_fg_rennstart_gb1_plaus: InMeasurement, St_as_plaus_B_ftauf1_plaus: InMeasurement, St_as_plaus_B_gangwechsel_gs_plaus: InMeasurement, St_as_plaus_B_hkauf_plaus: InMeasurement, St_as_plaus_B_hz_plaus: InMeasurement, St_as_plaus_B_kl15_ep_plaus: InMeasurement, St_as_plaus_B_kupp1_plaus: InMeasurement, St_as_plaus_B_mhauf1_plaus: InMeasurement, St_as_plaus_B_schlok_plaus: InMeasurement, St_as_plaus_B_sochold_anf_plaus: InMeasurement, St_egsprog_kor: InMeasurement, St_mdinfo_s: InMeasurement, St_shift: InMeasurement, Tmot: InMeasurement, Var_hs: InMeasurement, BMWtchob_cw_GcUpDwnAtTrig_C: BigDecimal, BMWtchob_cw_GearAtTrig_T: CurveType[String, BigDecimal], BMWtchob_cw_GearMtTrig_C: BigDecimal, BMWtchob_cw_TqInfoAtTrig_C: BigDecimal, BMWtchob_p_DifGraTrigMin_C: BigDecimal, BMWtchob_p_DifTrigMin_C: BigDecimal, BMWtchob_rat_AccrAtTrigMin_C: BigDecimal, BMWtchob_rat_AccrMtTrigMin_C: BigDecimal, BMWtchob_t_EngMin_C: BigDecimal, BMWtchob_tq_GraTrigMin_C: BigDecimal, BMWtchob_tq_TrigMin_C: BigDecimal, BMWtchob_b_Trig_bo: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchOb_Trig_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchob_cw_GcUpDwnAtTrig_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchob_cw_GcUpDwnAtTrig_C")
val BMWtchob_cw_GearAtTrig_T: CurveType[String, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchob_cw_GearAtTrig_T")
val BMWtchob_cw_GearMtTrig_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchob_cw_GearMtTrig_C")
val BMWtchob_cw_TqInfoAtTrig_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchob_cw_TqInfoAtTrig_C")
val BMWtchob_p_DifGraTrigMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchob_p_DifGraTrigMin_C")
val BMWtchob_p_DifTrigMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchob_p_DifTrigMin_C")
val BMWtchob_rat_AccrAtTrigMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchob_rat_AccrAtTrigMin_C")
val BMWtchob_rat_AccrMtTrigMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchob_rat_AccrMtTrigMin_C")
val BMWtchob_t_EngMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchob_t_EngMin_C")
val BMWtchob_tq_GraTrigMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchob_tq_GraTrigMin_C")
val BMWtchob_tq_TrigMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchob_tq_TrigMin_C")
val BMWtchbas_p_DifGra_sw: InMeasurement = a2lBin.measurement("BMWtchbas_p_DifGra_sw")
val BMWtchbas_p_Dif_sw: InMeasurement = a2lBin.measurement("BMWtchbas_p_Dif_sw")
val BMWtchco_b_Clc10MilliSec2_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Clc10MilliSec2_bo")
val BMWtchdiag_b_rf_Lim_bo: InMeasurement = a2lBin.measurement("BMWtchdiag_b_rf_Lim_bo")
val BMWtchdiag_b_tq_Lim_bo: InMeasurement = a2lBin.measurement("BMWtchdiag_b_tq_Lim_bo")
val Gangi_stat: InMeasurement = a2lBin.measurement("Gangi_stat")
val Mdk_wunsch: InMeasurement = a2lBin.measurement("Mdk_wunsch")
val Mdk_wunsch_grad: InMeasurement = a2lBin.measurement("Mdk_wunsch_grad")
val Pwg_ist: InMeasurement = a2lBin.measurement("Pwg_ist")
val St_as_plaus: InMeasurement = a2lBin.measurement("St_as_plaus")
val St_as_plaus_B_anhang_plaus: InMeasurement = a2lBin.measurement("St_as_plaus.B_anhang_plaus")
val St_as_plaus_B_csn_egs_plaus: InMeasurement = a2lBin.measurement("St_as_plaus.B_csn_egs_plaus")
val St_as_plaus_B_egs_khl1_plaus: InMeasurement = a2lBin.measurement("St_as_plaus.B_egs_khl1_plaus")
val St_as_plaus_B_egs_khl2_plaus: InMeasurement = a2lBin.measurement("St_as_plaus.B_egs_khl2_plaus")
val St_as_plaus_B_fagurt_plaus: InMeasurement = a2lBin.measurement("St_as_plaus.B_fagurt_plaus")
val St_as_plaus_B_fg_overboost_em2_plaus: InMeasurement = a2lBin.measurement("St_as_plaus.B_fg_overboost_em2_plaus")
val St_as_plaus_B_fg_rennstart_gb1_plaus: InMeasurement = a2lBin.measurement("St_as_plaus.B_fg_rennstart_gb1_plaus")
val St_as_plaus_B_ftauf1_plaus: InMeasurement = a2lBin.measurement("St_as_plaus.B_ftauf1_plaus")
val St_as_plaus_B_gangwechsel_gs_plaus: InMeasurement = a2lBin.measurement("St_as_plaus.B_gangwechsel_gs_plaus")
val St_as_plaus_B_hkauf_plaus: InMeasurement = a2lBin.measurement("St_as_plaus.B_hkauf_plaus")
val St_as_plaus_B_hz_plaus: InMeasurement = a2lBin.measurement("St_as_plaus.B_hz_plaus")
val St_as_plaus_B_kl15_ep_plaus: InMeasurement = a2lBin.measurement("St_as_plaus.B_kl15_ep_plaus")
val St_as_plaus_B_kupp1_plaus: InMeasurement = a2lBin.measurement("St_as_plaus.B_kupp1_plaus")
val St_as_plaus_B_mhauf1_plaus: InMeasurement = a2lBin.measurement("St_as_plaus.B_mhauf1_plaus")
val St_as_plaus_B_schlok_plaus: InMeasurement = a2lBin.measurement("St_as_plaus.B_schlok_plaus")
val St_as_plaus_B_sochold_anf_plaus: InMeasurement = a2lBin.measurement("St_as_plaus.B_sochold_anf_plaus")
val St_egsprog_kor: InMeasurement = a2lBin.measurement("St_egsprog_kor")
val St_mdinfo_s: InMeasurement = a2lBin.measurement("St_mdinfo_s")
val St_shift: InMeasurement = a2lBin.measurement("St_shift")
val Tmot: InMeasurement = a2lBin.measurement("Tmot")
val Var_hs: InMeasurement = a2lBin.measurement("Var_hs")
val BMWtchob_b_Trig_bo: OutMeasurement = a2lBin.measurement("BMWtchob_b_Trig_bo")

  BMW_MOD_TchOb_Trig_10ms(BMWtchbas_p_DifGra_sw, BMWtchbas_p_Dif_sw, BMWtchco_b_Clc10MilliSec2_bo, BMWtchdiag_b_rf_Lim_bo, BMWtchdiag_b_tq_Lim_bo, Gangi_stat, Mdk_wunsch, Mdk_wunsch_grad, Pwg_ist, St_as_plaus, St_as_plaus_B_anhang_plaus, St_as_plaus_B_csn_egs_plaus, St_as_plaus_B_egs_khl1_plaus, St_as_plaus_B_egs_khl2_plaus, St_as_plaus_B_fagurt_plaus, St_as_plaus_B_fg_overboost_em2_plaus, St_as_plaus_B_fg_rennstart_gb1_plaus, St_as_plaus_B_ftauf1_plaus, St_as_plaus_B_gangwechsel_gs_plaus, St_as_plaus_B_hkauf_plaus, St_as_plaus_B_hz_plaus, St_as_plaus_B_kl15_ep_plaus, St_as_plaus_B_kupp1_plaus, St_as_plaus_B_mhauf1_plaus, St_as_plaus_B_schlok_plaus, St_as_plaus_B_sochold_anf_plaus, St_egsprog_kor, St_mdinfo_s, St_shift, Tmot, Var_hs, BMWtchob_cw_GcUpDwnAtTrig_C, BMWtchob_cw_GearAtTrig_T, BMWtchob_cw_GearMtTrig_C, BMWtchob_cw_TqInfoAtTrig_C, BMWtchob_p_DifGraTrigMin_C, BMWtchob_p_DifTrigMin_C, BMWtchob_rat_AccrAtTrigMin_C, BMWtchob_rat_AccrMtTrigMin_C, BMWtchob_t_EngMin_C, BMWtchob_tq_GraTrigMin_C, BMWtchob_tq_TrigMin_C, BMWtchob_b_Trig_bo)
}


def BMW_MOD_TchOut_10ms(BMWeisy_b_DiagAirFlowCoaEnd_bo: InMeasurement, BMWeng_b_CkMovg_bo: InMeasurement, BMWeng_b_StrEnd_bo: InMeasurement, BMWlhmplau_b_StrSysAcv_bo: InMeasurement, BMWtchbas_b_Noise_bo: InMeasurement, BMWtchbov_b_Acv_bo: InMeasurement, BMWtchco_b_Acv_bo: InMeasurement, BMWtchco_b_Clc10MilliSec_bo: InMeasurement, BMWtchco_b_CthIdl_bo: InMeasurement, BMWtchco_b_LnkAftCmprSlv_bo: InMeasurement, BMWtchco_b_Rdy_bo: InMeasurement, BMWtchctr_pct_Wg_uw: InMeasurement, BMWtchsp_mf_Ex_uw: InMeasurement, BMWtchsvc_b_Acv_bo: InMeasurement, BMWtchsvc_pct_Wg_uw: InMeasurement, BMWtqe_b_Dfco_bo: InMeasurement, Nkw: InMeasurement, St_as_in_fahrer2: InMeasurement, St_as_in_fahrer2_B_bls_fast: InMeasurement, St_as_in_fahrer2_B_bls_pred: InMeasurement, St_as_in_fahrer2_B_ll_fzg: InMeasurement, St_as_in_fahrer2_B_rennstart_as: InMeasurement, St_as_in_fahrer2_B_vl_fzg: InMeasurement, St_eisydiag_enahfmb_global: InMeasurement, St_eisydiag_enahfmb_global_B_eqpl_akt: InMeasurement, St_eisydiag_enahfmb_global_B_hfmplcd_akt: InMeasurement, St_eisydiag_enahfmb_global_B_hfmplll_akt: InMeasurement, BMWtchout_f_GasWg_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchout_pct_Dfco_C: BigDecimal, BMWtchout_pct_WgBlwOff_T: CurveType[BigDecimal, BigDecimal], BMWtchout_pct_WgCthIdl_C: BigDecimal, BMWtchout_pct_WgDfcoDiag_C: BigDecimal, BMWtchout_pct_WgMan_C: BigDecimal, BMWtchout_pct_WgNotRdy_C: BigDecimal, BMWtchout_pct_WgOfs_C: BigDecimal, BMWtchout_pct_WgStrt_C: BigDecimal, BMWtchout_pct_WgWait_C: BigDecimal, BMWtchout_swi_BlwOff_C: String, BMWtchout_swi_DfcoDiagWgOp_C: String, BMWtchout_swi_Dfco_C: String, BMWtchout_swi_PctWgMan_C: String, BMWtchout_ti_DlyDfcoDiagStr_C: BigDecimal, BMWtchout_f_GasWg_uw: OutMeasurement, BMWtchout_pct_Wg_uw: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchOut_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchout_f_GasWg_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchout_f_GasWg_M")
val BMWtchout_pct_Dfco_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchout_pct_Dfco_C")
val BMWtchout_pct_WgBlwOff_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchout_pct_WgBlwOff_T")
val BMWtchout_pct_WgCthIdl_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchout_pct_WgCthIdl_C")
val BMWtchout_pct_WgDfcoDiag_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchout_pct_WgDfcoDiag_C")
val BMWtchout_pct_WgMan_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchout_pct_WgMan_C")
val BMWtchout_pct_WgNotRdy_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchout_pct_WgNotRdy_C")
val BMWtchout_pct_WgOfs_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchout_pct_WgOfs_C")
val BMWtchout_pct_WgStrt_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchout_pct_WgStrt_C")
val BMWtchout_pct_WgWait_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchout_pct_WgWait_C")
val BMWtchout_swi_BlwOff_C: String = a2lBin.readCharacteristicWithCast("BMWtchout_swi_BlwOff_C")
val BMWtchout_swi_DfcoDiagWgOp_C: String = a2lBin.readCharacteristicWithCast("BMWtchout_swi_DfcoDiagWgOp_C")
val BMWtchout_swi_Dfco_C: String = a2lBin.readCharacteristicWithCast("BMWtchout_swi_Dfco_C")
val BMWtchout_swi_PctWgMan_C: String = a2lBin.readCharacteristicWithCast("BMWtchout_swi_PctWgMan_C")
val BMWtchout_ti_DlyDfcoDiagStr_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchout_ti_DlyDfcoDiagStr_C")
val BMWeisy_b_DiagAirFlowCoaEnd_bo: InMeasurement = a2lBin.measurement("BMWeisy_b_DiagAirFlowCoaEnd_bo")
val BMWeng_b_CkMovg_bo: InMeasurement = a2lBin.measurement("BMWeng_b_CkMovg_bo")
val BMWeng_b_StrEnd_bo: InMeasurement = a2lBin.measurement("BMWeng_b_StrEnd_bo")
val BMWlhmplau_b_StrSysAcv_bo: InMeasurement = a2lBin.measurement("BMWlhmplau_b_StrSysAcv_bo")
val BMWtchbas_b_Noise_bo: InMeasurement = a2lBin.measurement("BMWtchbas_b_Noise_bo")
val BMWtchbov_b_Acv_bo: InMeasurement = a2lBin.measurement("BMWtchbov_b_Acv_bo")
val BMWtchco_b_Acv_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Acv_bo")
val BMWtchco_b_Clc10MilliSec_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Clc10MilliSec_bo")
val BMWtchco_b_CthIdl_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_CthIdl_bo")
val BMWtchco_b_LnkAftCmprSlv_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_LnkAftCmprSlv_bo")
val BMWtchco_b_Rdy_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Rdy_bo")
val BMWtchctr_pct_Wg_uw: InMeasurement = a2lBin.measurement("BMWtchctr_pct_Wg_uw")
val BMWtchsp_mf_Ex_uw: InMeasurement = a2lBin.measurement("BMWtchsp_mf_Ex_uw")
val BMWtchsvc_b_Acv_bo: InMeasurement = a2lBin.measurement("BMWtchsvc_b_Acv_bo")
val BMWtchsvc_pct_Wg_uw: InMeasurement = a2lBin.measurement("BMWtchsvc_pct_Wg_uw")
val BMWtqe_b_Dfco_bo: InMeasurement = a2lBin.measurement("BMWtqe_b_Dfco_bo")
val Nkw: InMeasurement = a2lBin.measurement("Nkw")
val St_as_in_fahrer2: InMeasurement = a2lBin.measurement("St_as_in_fahrer2")
val St_as_in_fahrer2_B_bls_fast: InMeasurement = a2lBin.measurement("St_as_in_fahrer2.B_bls_fast")
val St_as_in_fahrer2_B_bls_pred: InMeasurement = a2lBin.measurement("St_as_in_fahrer2.B_bls_pred")
val St_as_in_fahrer2_B_ll_fzg: InMeasurement = a2lBin.measurement("St_as_in_fahrer2.B_ll_fzg")
val St_as_in_fahrer2_B_rennstart_as: InMeasurement = a2lBin.measurement("St_as_in_fahrer2.B_rennstart_as")
val St_as_in_fahrer2_B_vl_fzg: InMeasurement = a2lBin.measurement("St_as_in_fahrer2.B_vl_fzg")
val St_eisydiag_enahfmb_global: InMeasurement = a2lBin.measurement("St_eisydiag_enahfmb_global")
val St_eisydiag_enahfmb_global_B_eqpl_akt: InMeasurement = a2lBin.measurement("St_eisydiag_enahfmb_global.B_eqpl_akt")
val St_eisydiag_enahfmb_global_B_hfmplcd_akt: InMeasurement = a2lBin.measurement("St_eisydiag_enahfmb_global.B_hfmplcd_akt")
val St_eisydiag_enahfmb_global_B_hfmplll_akt: InMeasurement = a2lBin.measurement("St_eisydiag_enahfmb_global.B_hfmplll_akt")
val BMWtchout_f_GasWg_uw: OutMeasurement = a2lBin.measurement("BMWtchout_f_GasWg_uw")
val BMWtchout_pct_Wg_uw: OutMeasurement = a2lBin.measurement("BMWtchout_pct_Wg_uw")

  BMW_MOD_TchOut_10ms(BMWeisy_b_DiagAirFlowCoaEnd_bo, BMWeng_b_CkMovg_bo, BMWeng_b_StrEnd_bo, BMWlhmplau_b_StrSysAcv_bo, BMWtchbas_b_Noise_bo, BMWtchbov_b_Acv_bo, BMWtchco_b_Acv_bo, BMWtchco_b_Clc10MilliSec_bo, BMWtchco_b_CthIdl_bo, BMWtchco_b_LnkAftCmprSlv_bo, BMWtchco_b_Rdy_bo, BMWtchctr_pct_Wg_uw, BMWtchsp_mf_Ex_uw, BMWtchsvc_b_Acv_bo, BMWtchsvc_pct_Wg_uw, BMWtqe_b_Dfco_bo, Nkw, St_as_in_fahrer2, St_as_in_fahrer2_B_bls_fast, St_as_in_fahrer2_B_bls_pred, St_as_in_fahrer2_B_ll_fzg, St_as_in_fahrer2_B_rennstart_as, St_as_in_fahrer2_B_vl_fzg, St_eisydiag_enahfmb_global, St_eisydiag_enahfmb_global_B_eqpl_akt, St_eisydiag_enahfmb_global_B_hfmplcd_akt, St_eisydiag_enahfmb_global_B_hfmplll_akt, BMWtchout_f_GasWg_M, BMWtchout_pct_Dfco_C, BMWtchout_pct_WgBlwOff_T, BMWtchout_pct_WgCthIdl_C, BMWtchout_pct_WgDfcoDiag_C, BMWtchout_pct_WgMan_C, BMWtchout_pct_WgNotRdy_C, BMWtchout_pct_WgOfs_C, BMWtchout_pct_WgStrt_C, BMWtchout_pct_WgWait_C, BMWtchout_swi_BlwOff_C, BMWtchout_swi_DfcoDiagWgOp_C, BMWtchout_swi_Dfco_C, BMWtchout_swi_PctWgMan_C, BMWtchout_ti_DlyDfcoDiagStr_C, BMWtchout_f_GasWg_uw, BMWtchout_pct_Wg_uw)
}


def BMW_MOD_TchScv_Dyn_10ms(BMWext_t_GasInCatAvg_sw: InMeasurement, BMWtchbas_p_DifGra_sw: InMeasurement, BMWtchbas_p_Dif_sw: InMeasurement, BMWtchco_b_Clc10MilliSec_bo: InMeasurement, BMWtchscv_b_Ena_bo: InMeasurement, BMWtchsp_p_ReqDyn_sw: InMeasurement, Fho: InMeasurement, Nkw: InMeasurement, Rfv_vns: InMeasurement, Tmot: InMeasurement, BMWtchscv_n_DynHysMax_C: BigDecimal, BMWtchscv_n_DynMax_C: BigDecimal, BMWtchscv_p_DifDynDeac_T: CurveType[BigDecimal, BigDecimal], BMWtchscv_p_DifMin_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchscv_p_ReqDynMin_C: BigDecimal, BMWtchscv_rf_DynHysMin_C: BigDecimal, BMWtchscv_rf_DynMin_C: BigDecimal, BMWtchscv_swi_DynRstNotEna_C: String, BMWtchscv_t_EgCatRst_C: BigDecimal, BMWtchscv_t_EngMin_C: BigDecimal, BMWtchscv_t_ExGasCatMax_C: BigDecimal, BMWtchscv_ti_DlyDynDeac_T: CurveType[BigDecimal, BigDecimal], BMWtchscv_ti_DynLock_C: BigDecimal, BMWtchscv_ti_DynMax_T: CurveType[BigDecimal, BigDecimal], BMWtchscv_b_Dyn_bo: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchScv_Dyn_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchscv_n_DynHysMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchscv_n_DynHysMax_C")
val BMWtchscv_n_DynMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchscv_n_DynMax_C")
val BMWtchscv_p_DifDynDeac_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchscv_p_DifDynDeac_T")
val BMWtchscv_p_DifMin_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchscv_p_DifMin_M")
val BMWtchscv_p_ReqDynMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchscv_p_ReqDynMin_C")
val BMWtchscv_rf_DynHysMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchscv_rf_DynHysMin_C")
val BMWtchscv_rf_DynMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchscv_rf_DynMin_C")
val BMWtchscv_swi_DynRstNotEna_C: String = a2lBin.readCharacteristicWithCast("BMWtchscv_swi_DynRstNotEna_C")
val BMWtchscv_t_EgCatRst_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchscv_t_EgCatRst_C")
val BMWtchscv_t_EngMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchscv_t_EngMin_C")
val BMWtchscv_t_ExGasCatMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchscv_t_ExGasCatMax_C")
val BMWtchscv_ti_DlyDynDeac_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchscv_ti_DlyDynDeac_T")
val BMWtchscv_ti_DynLock_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchscv_ti_DynLock_C")
val BMWtchscv_ti_DynMax_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchscv_ti_DynMax_T")
val BMWext_t_GasInCatAvg_sw: InMeasurement = a2lBin.measurement("BMWext_t_GasInCatAvg_sw")
val BMWtchbas_p_DifGra_sw: InMeasurement = a2lBin.measurement("BMWtchbas_p_DifGra_sw")
val BMWtchbas_p_Dif_sw: InMeasurement = a2lBin.measurement("BMWtchbas_p_Dif_sw")
val BMWtchco_b_Clc10MilliSec_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Clc10MilliSec_bo")
val BMWtchscv_b_Ena_bo: InMeasurement = a2lBin.measurement("BMWtchscv_b_Ena_bo")
val BMWtchsp_p_ReqDyn_sw: InMeasurement = a2lBin.measurement("BMWtchsp_p_ReqDyn_sw")
val Fho: InMeasurement = a2lBin.measurement("Fho")
val Nkw: InMeasurement = a2lBin.measurement("Nkw")
val Rfv_vns: InMeasurement = a2lBin.measurement("Rfv_vns")
val Tmot: InMeasurement = a2lBin.measurement("Tmot")
val BMWtchscv_b_Dyn_bo: OutMeasurement = a2lBin.measurement("BMWtchscv_b_Dyn_bo")

  BMW_MOD_TchScv_Dyn_10ms(BMWext_t_GasInCatAvg_sw, BMWtchbas_p_DifGra_sw, BMWtchbas_p_Dif_sw, BMWtchco_b_Clc10MilliSec_bo, BMWtchscv_b_Ena_bo, BMWtchsp_p_ReqDyn_sw, Fho, Nkw, Rfv_vns, Tmot, BMWtchscv_n_DynHysMax_C, BMWtchscv_n_DynMax_C, BMWtchscv_p_DifDynDeac_T, BMWtchscv_p_DifMin_M, BMWtchscv_p_ReqDynMin_C, BMWtchscv_rf_DynHysMin_C, BMWtchscv_rf_DynMin_C, BMWtchscv_swi_DynRstNotEna_C, BMWtchscv_t_EgCatRst_C, BMWtchscv_t_EngMin_C, BMWtchscv_t_ExGasCatMax_C, BMWtchscv_ti_DlyDynDeac_T, BMWtchscv_ti_DynLock_C, BMWtchscv_ti_DynMax_T, BMWtchscv_b_Dyn_bo)
}


def BMW_MOD_TchScv_Ena_10ms(BMWign_ag_OutMv_sw: InMeasurement, BMWpim_b_PrevnScavProh_bo: InMeasurement, BMWtchco_b_Clc10MilliSec_bo: InMeasurement, BMWtchscv_b_Wish_bo: InMeasurement, Bed_lamson_ok_xab: InMeasurement, F_basis: InMeasurement, Nkw: InMeasurement, Rf: InMeasurement, St_kr: InMeasurement, St_kr_B_krdws: InMeasurement, St_kr_B_krndy: InMeasurement, St_kr_B_warml_zykl: InMeasurement, Zwstat: InMeasurement, BMWtchscv_ag_IgRtd_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchscv_fac_VnsMax_C: BigDecimal, BMWtchscv_swi_KnkDetErrOff_C: String, BMWtchscv_ti_DebDeac_C: BigDecimal, K_ANZAHL_ABGBAENKE: BigDecimal, BMWtchscv_b_Ena_bo: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchScv_Ena_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchscv_ag_IgRtd_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchscv_ag_IgRtd_M")
val BMWtchscv_fac_VnsMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchscv_fac_VnsMax_C")
val BMWtchscv_swi_KnkDetErrOff_C: String = a2lBin.readCharacteristicWithCast("BMWtchscv_swi_KnkDetErrOff_C")
val BMWtchscv_ti_DebDeac_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchscv_ti_DebDeac_C")
val K_ANZAHL_ABGBAENKE: BigDecimal = a2lBin.readCharacteristicWithCast("K_ANZAHL_ABGBAENKE")
val BMWign_ag_OutMv_sw: InMeasurement = a2lBin.measurement("BMWign_ag_OutMv_sw")
val BMWpim_b_PrevnScavProh_bo: InMeasurement = a2lBin.measurement("BMWpim_b_PrevnScavProh_bo")
val BMWtchco_b_Clc10MilliSec_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Clc10MilliSec_bo")
val BMWtchscv_b_Wish_bo: InMeasurement = a2lBin.measurement("BMWtchscv_b_Wish_bo")
val Bed_lamson_ok_xab: InMeasurement = a2lBin.measurement("Bed_lamson_ok_xab")
val F_basis: InMeasurement = a2lBin.measurement("F_basis")
val Nkw: InMeasurement = a2lBin.measurement("Nkw")
val Rf: InMeasurement = a2lBin.measurement("Rf")
val St_kr: InMeasurement = a2lBin.measurement("St_kr")
val St_kr_B_krdws: InMeasurement = a2lBin.measurement("St_kr.B_krdws")
val St_kr_B_krndy: InMeasurement = a2lBin.measurement("St_kr.B_krndy")
val St_kr_B_warml_zykl: InMeasurement = a2lBin.measurement("St_kr.B_warml_zykl")
val Zwstat: InMeasurement = a2lBin.measurement("Zwstat")
val BMWtchscv_b_Ena_bo: OutMeasurement = a2lBin.measurement("BMWtchscv_b_Ena_bo")

  BMW_MOD_TchScv_Ena_10ms(BMWign_ag_OutMv_sw, BMWpim_b_PrevnScavProh_bo, BMWtchco_b_Clc10MilliSec_bo, BMWtchscv_b_Wish_bo, Bed_lamson_ok_xab, F_basis, Nkw, Rf, St_kr, St_kr_B_krdws, St_kr_B_krndy, St_kr_B_warml_zykl, Zwstat, BMWtchscv_ag_IgRtd_M, BMWtchscv_fac_VnsMax_C, BMWtchscv_swi_KnkDetErrOff_C, BMWtchscv_ti_DebDeac_C, K_ANZAHL_ABGBAENKE, BMWtchscv_b_Ena_bo)
}


def BMW_MOD_TchScv_Fac_10ms(BMWfcomain_cw_Perm_uw: InMeasurement, BMWtchco_b_Clc10MilliSec_bo: InMeasurement, BMWtchco_st_Opm_ub: InMeasurement, BMWtchscv_b_Dyn_bo: InMeasurement, BMWtchscv_b_Ena_bo: InMeasurement, Dla_wup: InMeasurement, Nkw: InMeasurement, Rfv_vns: InMeasurement, St_progression: InMeasurement, St_progression_B_eco: InMeasurement, St_progression_B_sport: InMeasurement, Tans: InMeasurement, Tmot: InMeasurement, BMWtchscv_fac_DynEco_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchscv_fac_Dyn_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchscv_fac_GraDec_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchscv_fac_GraInc_C: BigDecimal, BMWtchscv_fac_ReqOff_C: BigDecimal, BMWtchscv_fac_ReqOn_C: BigDecimal, BMWtchscv_fac_StatEco_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchscv_fac_Stat_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchscv_fac_WupCrtn_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchscv_fac_Wup_T: CurveType[BigDecimal, BigDecimal], BMWtchscv_swi_ReqMan_C: String, BMWtchscv_swi_StatEcoInh_C: String, BMWtchscv_b_Req_bo: OutMeasurement, BMWtchscv_b_Wish_bo: OutMeasurement, BMWtchscv_fac_Acv_ub: OutMeasurement, BMWtchscv_fac__ub: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchScv_Fac_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchscv_fac_DynEco_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchscv_fac_DynEco_M")
val BMWtchscv_fac_Dyn_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchscv_fac_Dyn_M")
val BMWtchscv_fac_GraDec_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchscv_fac_GraDec_M")
val BMWtchscv_fac_GraInc_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchscv_fac_GraInc_C")
val BMWtchscv_fac_ReqOff_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchscv_fac_ReqOff_C")
val BMWtchscv_fac_ReqOn_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchscv_fac_ReqOn_C")
val BMWtchscv_fac_StatEco_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchscv_fac_StatEco_M")
val BMWtchscv_fac_Stat_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchscv_fac_Stat_M")
val BMWtchscv_fac_WupCrtn_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchscv_fac_WupCrtn_M")
val BMWtchscv_fac_Wup_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchscv_fac_Wup_T")
val BMWtchscv_swi_ReqMan_C: String = a2lBin.readCharacteristicWithCast("BMWtchscv_swi_ReqMan_C")
val BMWtchscv_swi_StatEcoInh_C: String = a2lBin.readCharacteristicWithCast("BMWtchscv_swi_StatEcoInh_C")
val BMWfcomain_cw_Perm_uw: InMeasurement = a2lBin.measurement("BMWfcomain_cw_Perm_uw")
val BMWtchco_b_Clc10MilliSec_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Clc10MilliSec_bo")
val BMWtchco_st_Opm_ub: InMeasurement = a2lBin.measurement("BMWtchco_st_Opm_ub")
val BMWtchscv_b_Dyn_bo: InMeasurement = a2lBin.measurement("BMWtchscv_b_Dyn_bo")
val BMWtchscv_b_Ena_bo: InMeasurement = a2lBin.measurement("BMWtchscv_b_Ena_bo")
val Dla_wup: InMeasurement = a2lBin.measurement("Dla_wup")
val Nkw: InMeasurement = a2lBin.measurement("Nkw")
val Rfv_vns: InMeasurement = a2lBin.measurement("Rfv_vns")
val St_progression: InMeasurement = a2lBin.measurement("St_progression")
val St_progression_B_eco: InMeasurement = a2lBin.measurement("St_progression.B_eco")
val St_progression_B_sport: InMeasurement = a2lBin.measurement("St_progression.B_sport")
val Tans: InMeasurement = a2lBin.measurement("Tans")
val Tmot: InMeasurement = a2lBin.measurement("Tmot")
val BMWtchscv_b_Req_bo: OutMeasurement = a2lBin.measurement("BMWtchscv_b_Req_bo")
val BMWtchscv_b_Wish_bo: OutMeasurement = a2lBin.measurement("BMWtchscv_b_Wish_bo")
val BMWtchscv_fac_Acv_ub: OutMeasurement = a2lBin.measurement("BMWtchscv_fac_Acv_ub")
val BMWtchscv_fac__ub: OutMeasurement = a2lBin.measurement("BMWtchscv_fac__ub")

  BMW_MOD_TchScv_Fac_10ms(BMWfcomain_cw_Perm_uw, BMWtchco_b_Clc10MilliSec_bo, BMWtchco_st_Opm_ub, BMWtchscv_b_Dyn_bo, BMWtchscv_b_Ena_bo, Dla_wup, Nkw, Rfv_vns, St_progression, St_progression_B_eco, St_progression_B_sport, Tans, Tmot, BMWtchscv_fac_DynEco_M, BMWtchscv_fac_Dyn_M, BMWtchscv_fac_GraDec_M, BMWtchscv_fac_GraInc_C, BMWtchscv_fac_ReqOff_C, BMWtchscv_fac_ReqOn_C, BMWtchscv_fac_StatEco_M, BMWtchscv_fac_Stat_M, BMWtchscv_fac_WupCrtn_M, BMWtchscv_fac_Wup_T, BMWtchscv_swi_ReqMan_C, BMWtchscv_swi_StatEcoInh_C, BMWtchscv_b_Req_bo, BMWtchscv_b_Wish_bo, BMWtchscv_fac_Acv_ub, BMWtchscv_fac__ub)
}


def BMW_MOD_TchSp_P_10ms(BMWeisy_b_PSnsrPreThrPlau_bo: InMeasurement, BMWeisy_p_PreThrPlau_uw: InMeasurement, BMWgpfdiag_rf_LimPclFil_sw: InMeasurement, BMWgpfp_b_PclFil_bo: InMeasurement, BMWtchbas_b_RaceStr_bo: InMeasurement, BMWtchbas_p_BefCmpr_uw: InMeasurement, BMWtchco_b_Clc10MilliSec_bo: InMeasurement, BMWtchdiag_b_rf_Lim_bo: InMeasurement, BMWtchdiag_b_tq_Lim_bo: InMeasurement, BMWtchdiag_rf_MaxPBefThrHi_sw: InMeasurement, BMWtchsp_mf_CmprNorm_uw: InMeasurement, BMWtchsp_volf_Ico_uw: InMeasurement, BMWtchtbc_t_BefCmpr_sw: InMeasurement, Drft_uk: InMeasurement, Fupsrf_kl: InMeasurement, Fupsrf_kor_f: InMeasurement, Nkw: InMeasurement, Pirg_kl: InMeasurement, Pirg_kor_f: InMeasurement, Pld_soll: InMeasurement, Pumg: InMeasurement, Rf_mdk_max: InMeasurement, Rf_vlsaug_max: InMeasurement, BMWtchsp_cw_1_C: BigDecimal, BMWtchsp_cw_RfLimAcv_C: BigDecimal, BMWtchsp_fac_FadeMdlFlLim_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchsp_fac_FilPDifReqMax_C: BigDecimal, BMWtchsp_fac_FilPDyn_C: BigDecimal, BMWtchsp_fac_FilPRatCmpr_T: CurveType[BigDecimal, BigDecimal], BMWtchsp_fac_rf_MaxSmaMdl_C: BigDecimal, BMWtchsp_p_DifIco_T: CurveType[BigDecimal, BigDecimal], BMWtchsp_p_OfsMaxHys_C: BigDecimal, BMWtchsp_p_OfsMax_C: BigDecimal, BMWtchsp_p_ReqMaxSnsrErr_C: BigDecimal, BMWtchsp_p_ReqMax_C: BigDecimal, BMWtchsp_rat_p_CmprMax_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchsp_rat_p_CmprPmp_T: CurveType[BigDecimal, BigDecimal], BMWtchsp_rf_Liho_T: CurveType[BigDecimal, BigDecimal], BMWtchsp_rf_LimThd_C: BigDecimal, BMWtchsp_swi_PSpRace_C: String, BMWtchsp_b_RfLimAcv_bo: OutMeasurement, BMWtchsp_p_DifIco_uw: OutMeasurement, BMWtchsp_p_ReqDyn_sw: OutMeasurement, BMWtchsp_p_Req_uw: OutMeasurement, BMWtchsp_rat_p_CmprLim_uw: OutMeasurement, BMWtchsp_rat_p_Cmpr_uw: OutMeasurement, Rf_max_pldmax: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchSp_P_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchsp_cw_1_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsp_cw_1_C")
val BMWtchsp_cw_RfLimAcv_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsp_cw_RfLimAcv_C")
val BMWtchsp_fac_FadeMdlFlLim_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchsp_fac_FadeMdlFlLim_M")
val BMWtchsp_fac_FilPDifReqMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsp_fac_FilPDifReqMax_C")
val BMWtchsp_fac_FilPDyn_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsp_fac_FilPDyn_C")
val BMWtchsp_fac_FilPRatCmpr_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchsp_fac_FilPRatCmpr_T")
val BMWtchsp_fac_rf_MaxSmaMdl_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsp_fac_rf_MaxSmaMdl_C")
val BMWtchsp_p_DifIco_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchsp_p_DifIco_T")
val BMWtchsp_p_OfsMaxHys_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsp_p_OfsMaxHys_C")
val BMWtchsp_p_OfsMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsp_p_OfsMax_C")
val BMWtchsp_p_ReqMaxSnsrErr_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsp_p_ReqMaxSnsrErr_C")
val BMWtchsp_p_ReqMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsp_p_ReqMax_C")
val BMWtchsp_rat_p_CmprMax_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchsp_rat_p_CmprMax_M")
val BMWtchsp_rat_p_CmprPmp_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchsp_rat_p_CmprPmp_T")
val BMWtchsp_rf_Liho_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchsp_rf_Liho_T")
val BMWtchsp_rf_LimThd_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsp_rf_LimThd_C")
val BMWtchsp_swi_PSpRace_C: String = a2lBin.readCharacteristicWithCast("BMWtchsp_swi_PSpRace_C")
val BMWeisy_b_PSnsrPreThrPlau_bo: InMeasurement = a2lBin.measurement("BMWeisy_b_PSnsrPreThrPlau_bo")
val BMWeisy_p_PreThrPlau_uw: InMeasurement = a2lBin.measurement("BMWeisy_p_PreThrPlau_uw")
val BMWgpfdiag_rf_LimPclFil_sw: InMeasurement = a2lBin.measurement("BMWgpfdiag_rf_LimPclFil_sw")
val BMWgpfp_b_PclFil_bo: InMeasurement = a2lBin.measurement("BMWgpfp_b_PclFil_bo")
val BMWtchbas_b_RaceStr_bo: InMeasurement = a2lBin.measurement("BMWtchbas_b_RaceStr_bo")
val BMWtchbas_p_BefCmpr_uw: InMeasurement = a2lBin.measurement("BMWtchbas_p_BefCmpr_uw")
val BMWtchco_b_Clc10MilliSec_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Clc10MilliSec_bo")
val BMWtchdiag_b_rf_Lim_bo: InMeasurement = a2lBin.measurement("BMWtchdiag_b_rf_Lim_bo")
val BMWtchdiag_b_tq_Lim_bo: InMeasurement = a2lBin.measurement("BMWtchdiag_b_tq_Lim_bo")
val BMWtchdiag_rf_MaxPBefThrHi_sw: InMeasurement = a2lBin.measurement("BMWtchdiag_rf_MaxPBefThrHi_sw")
val BMWtchsp_mf_CmprNorm_uw: InMeasurement = a2lBin.measurement("BMWtchsp_mf_CmprNorm_uw")
val BMWtchsp_volf_Ico_uw: InMeasurement = a2lBin.measurement("BMWtchsp_volf_Ico_uw")
val BMWtchtbc_t_BefCmpr_sw: InMeasurement = a2lBin.measurement("BMWtchtbc_t_BefCmpr_sw")
val Drft_uk: InMeasurement = a2lBin.measurement("Drft_uk")
val Fupsrf_kl: InMeasurement = a2lBin.measurement("Fupsrf_kl")
val Fupsrf_kor_f: InMeasurement = a2lBin.measurement("Fupsrf_kor_f")
val Nkw: InMeasurement = a2lBin.measurement("Nkw")
val Pirg_kl: InMeasurement = a2lBin.measurement("Pirg_kl")
val Pirg_kor_f: InMeasurement = a2lBin.measurement("Pirg_kor_f")
val Pld_soll: InMeasurement = a2lBin.measurement("Pld_soll")
val Pumg: InMeasurement = a2lBin.measurement("Pumg")
val Rf_mdk_max: InMeasurement = a2lBin.measurement("Rf_mdk_max")
val Rf_vlsaug_max: InMeasurement = a2lBin.measurement("Rf_vlsaug_max")
val BMWtchsp_b_RfLimAcv_bo: OutMeasurement = a2lBin.measurement("BMWtchsp_b_RfLimAcv_bo")
val BMWtchsp_p_DifIco_uw: OutMeasurement = a2lBin.measurement("BMWtchsp_p_DifIco_uw")
val BMWtchsp_p_ReqDyn_sw: OutMeasurement = a2lBin.measurement("BMWtchsp_p_ReqDyn_sw")
val BMWtchsp_p_Req_uw: OutMeasurement = a2lBin.measurement("BMWtchsp_p_Req_uw")
val BMWtchsp_rat_p_CmprLim_uw: OutMeasurement = a2lBin.measurement("BMWtchsp_rat_p_CmprLim_uw")
val BMWtchsp_rat_p_Cmpr_uw: OutMeasurement = a2lBin.measurement("BMWtchsp_rat_p_Cmpr_uw")
val Rf_max_pldmax: OutMeasurement = a2lBin.measurement("Rf_max_pldmax")

  BMW_MOD_TchSp_P_10ms(BMWeisy_b_PSnsrPreThrPlau_bo, BMWeisy_p_PreThrPlau_uw, BMWgpfdiag_rf_LimPclFil_sw, BMWgpfp_b_PclFil_bo, BMWtchbas_b_RaceStr_bo, BMWtchbas_p_BefCmpr_uw, BMWtchco_b_Clc10MilliSec_bo, BMWtchdiag_b_rf_Lim_bo, BMWtchdiag_b_tq_Lim_bo, BMWtchdiag_rf_MaxPBefThrHi_sw, BMWtchsp_mf_CmprNorm_uw, BMWtchsp_volf_Ico_uw, BMWtchtbc_t_BefCmpr_sw, Drft_uk, Fupsrf_kl, Fupsrf_kor_f, Nkw, Pirg_kl, Pirg_kor_f, Pld_soll, Pumg, Rf_mdk_max, Rf_vlsaug_max, BMWtchsp_cw_1_C, BMWtchsp_cw_RfLimAcv_C, BMWtchsp_fac_FadeMdlFlLim_M, BMWtchsp_fac_FilPDifReqMax_C, BMWtchsp_fac_FilPDyn_C, BMWtchsp_fac_FilPRatCmpr_T, BMWtchsp_fac_rf_MaxSmaMdl_C, BMWtchsp_p_DifIco_T, BMWtchsp_p_OfsMaxHys_C, BMWtchsp_p_OfsMax_C, BMWtchsp_p_ReqMaxSnsrErr_C, BMWtchsp_p_ReqMax_C, BMWtchsp_rat_p_CmprMax_M, BMWtchsp_rat_p_CmprPmp_T, BMWtchsp_rf_Liho_T, BMWtchsp_rf_LimThd_C, BMWtchsp_swi_PSpRace_C, BMWtchsp_b_RfLimAcv_bo, BMWtchsp_p_DifIco_uw, BMWtchsp_p_ReqDyn_sw, BMWtchsp_p_Req_uw, BMWtchsp_rat_p_CmprLim_uw, BMWtchsp_rat_p_Cmpr_uw, Rf_max_pldmax)
}


def BMW_MOD_TchSp_Volf_10ms(BMWeisy_p_PreThrPlau_uw: InMeasurement, BMWtchbas_p_BefCmpr_uw: InMeasurement, BMWtchbas_rat_p_BefCmpr_uw: InMeasurement, BMWtchco_b_Clc10MilliSec_bo: InMeasurement, BMWtchtbc_t_BefCmpr_sw: InMeasurement, Mk_kgh: InMeasurement, Mssol: InMeasurement, Pumg: InMeasurement, St_getrdaten: InMeasurement, St_getrdaten_B_gangwechsel_gs: InMeasurement, Tvdk: InMeasurement, BMWtchsp_fac_FadePBefTc_C: BigDecimal, BMWtchsp_fac_SqrtTBefTc_T: CurveType[BigDecimal, BigDecimal], BMWtchsp_fac_mf_FilCmprNormGc_C: BigDecimal, BMWtchsp_fac_mf_FilCmprNorm_T: CurveType[BigDecimal, BigDecimal], BMWtchsp_cw_1_C: BigDecimal, BMWtchsp_fac_mf_CmprNorm_uw: OutMeasurement, BMWtchsp_mf_CmprNorm_uw: OutMeasurement, BMWtchsp_mf_Ex_uw: OutMeasurement, BMWtchsp_volf_BefTc_uw: OutMeasurement, BMWtchsp_volf_Ico_uw: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchSp_Volf_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchsp_fac_FadePBefTc_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsp_fac_FadePBefTc_C")
val BMWtchsp_fac_SqrtTBefTc_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchsp_fac_SqrtTBefTc_T")
val BMWtchsp_fac_mf_FilCmprNormGc_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsp_fac_mf_FilCmprNormGc_C")
val BMWtchsp_fac_mf_FilCmprNorm_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchsp_fac_mf_FilCmprNorm_T")
val BMWtchsp_cw_1_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsp_cw_1_C")
val BMWeisy_p_PreThrPlau_uw: InMeasurement = a2lBin.measurement("BMWeisy_p_PreThrPlau_uw")
val BMWtchbas_p_BefCmpr_uw: InMeasurement = a2lBin.measurement("BMWtchbas_p_BefCmpr_uw")
val BMWtchbas_rat_p_BefCmpr_uw: InMeasurement = a2lBin.measurement("BMWtchbas_rat_p_BefCmpr_uw")
val BMWtchco_b_Clc10MilliSec_bo: InMeasurement = a2lBin.measurement("BMWtchco_b_Clc10MilliSec_bo")
val BMWtchtbc_t_BefCmpr_sw: InMeasurement = a2lBin.measurement("BMWtchtbc_t_BefCmpr_sw")
val Mk_kgh: InMeasurement = a2lBin.measurement("Mk_kgh")
val Mssol: InMeasurement = a2lBin.measurement("Mssol")
val Pumg: InMeasurement = a2lBin.measurement("Pumg")
val St_getrdaten: InMeasurement = a2lBin.measurement("St_getrdaten")
val St_getrdaten_B_gangwechsel_gs: InMeasurement = a2lBin.measurement("St_getrdaten.B_gangwechsel_gs")
val Tvdk: InMeasurement = a2lBin.measurement("Tvdk")
val BMWtchsp_fac_mf_CmprNorm_uw: OutMeasurement = a2lBin.measurement("BMWtchsp_fac_mf_CmprNorm_uw")
val BMWtchsp_mf_CmprNorm_uw: OutMeasurement = a2lBin.measurement("BMWtchsp_mf_CmprNorm_uw")
val BMWtchsp_mf_Ex_uw: OutMeasurement = a2lBin.measurement("BMWtchsp_mf_Ex_uw")
val BMWtchsp_volf_BefTc_uw: OutMeasurement = a2lBin.measurement("BMWtchsp_volf_BefTc_uw")
val BMWtchsp_volf_Ico_uw: OutMeasurement = a2lBin.measurement("BMWtchsp_volf_Ico_uw")

  BMW_MOD_TchSp_Volf_10ms(BMWeisy_p_PreThrPlau_uw, BMWtchbas_p_BefCmpr_uw, BMWtchbas_rat_p_BefCmpr_uw, BMWtchco_b_Clc10MilliSec_bo, BMWtchtbc_t_BefCmpr_sw, Mk_kgh, Mssol, Pumg, St_getrdaten, St_getrdaten_B_gangwechsel_gs, Tvdk, BMWtchsp_fac_FadePBefTc_C, BMWtchsp_fac_SqrtTBefTc_T, BMWtchsp_fac_mf_FilCmprNormGc_C, BMWtchsp_fac_mf_FilCmprNorm_T, BMWtchsp_cw_1_C, BMWtchsp_fac_mf_CmprNorm_uw, BMWtchsp_mf_CmprNorm_uw, BMWtchsp_mf_Ex_uw, BMWtchsp_volf_BefTc_uw, BMWtchsp_volf_Ico_uw)
}


def BMW_MOD_TchSvc_Acv_100ms(BMWchas_st_OpmAcv_ub: InMeasurement, BMWeisy_p_PreThrPlau_uw: InMeasurement, BMWtchsvc_b_Ena_bo: InMeasurement, BMWtchsvc_b_Req_bo: InMeasurement, Md_reib_vm: InMeasurement, Mdi_opt_lam: InMeasurement, Mdkw_soll: InMeasurement, Nkw: InMeasurement, Pumg: InMeasurement, Tans: InMeasurement, Tumg: InMeasurement, BMWtchsvc_fac_FilP_C: BigDecimal, BMWtchsvc_fac_p_Crtn_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchsvc_lam_Req_C: BigDecimal, BMWtchsvc_n_IdlDec_C: BigDecimal, BMWtchsvc_n_IdlInc_C: BigDecimal, BMWtchsvc_n_IdlMax_C: BigDecimal, BMWtchsvc_n_IdlMin_C: BigDecimal, BMWtchsvc_n_Idl_C: BigDecimal, BMWtchsvc_p_DeltMin_T: CurveType[BigDecimal, BigDecimal], BMWtchsvc_pct_WgOp_C: BigDecimal, BMWtchsvc_ti_CthAcvMax_C: BigDecimal, BMWtchsvc_ti_DebIdlNotOk_C: BigDecimal, BMWtchsvc_ti_DebPOk_C: BigDecimal, BMWtchsvc_ti_DebTqNotOk_C: BigDecimal, BMWtchsvc_ti_PChkMax_C: BigDecimal, BMWtchsvc_ti_TqMax_C: BigDecimal, BMWtchsvc_ti_n_IdlTot_C: BigDecimal, BMWtchsvc_tq_ResDec_C: BigDecimal, BMWtchsvc_tq_ResInc_C: BigDecimal, BMWtchsvc_tqi_AirTo_C: BigDecimal, BMWtchsvc_tqi_Air_T: CurveType[BigDecimal, BigDecimal], BMWtchsvc_b_Acv_bo: OutMeasurement, BMWtchsvc_b_CthReq_bo: OutMeasurement, BMWtchsvc_lam_Req_uw: OutMeasurement, BMWtchsvc_n_Idl_sw: OutMeasurement, BMWtchsvc_p_Delt_uw: OutMeasurement, BMWtchsvc_pct_Wg_uw: OutMeasurement, BMWtchsvc_st_Tstr_ub: OutMeasurement, BMWtchsvc_tq_Res_sw: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchSvc_Acv_100ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchsvc_fac_FilP_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsvc_fac_FilP_C")
val BMWtchsvc_fac_p_Crtn_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchsvc_fac_p_Crtn_M")
val BMWtchsvc_lam_Req_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsvc_lam_Req_C")
val BMWtchsvc_n_IdlDec_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsvc_n_IdlDec_C")
val BMWtchsvc_n_IdlInc_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsvc_n_IdlInc_C")
val BMWtchsvc_n_IdlMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsvc_n_IdlMax_C")
val BMWtchsvc_n_IdlMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsvc_n_IdlMin_C")
val BMWtchsvc_n_Idl_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsvc_n_Idl_C")
val BMWtchsvc_p_DeltMin_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchsvc_p_DeltMin_T")
val BMWtchsvc_pct_WgOp_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsvc_pct_WgOp_C")
val BMWtchsvc_ti_CthAcvMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsvc_ti_CthAcvMax_C")
val BMWtchsvc_ti_DebIdlNotOk_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsvc_ti_DebIdlNotOk_C")
val BMWtchsvc_ti_DebPOk_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsvc_ti_DebPOk_C")
val BMWtchsvc_ti_DebTqNotOk_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsvc_ti_DebTqNotOk_C")
val BMWtchsvc_ti_PChkMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsvc_ti_PChkMax_C")
val BMWtchsvc_ti_TqMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsvc_ti_TqMax_C")
val BMWtchsvc_ti_n_IdlTot_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsvc_ti_n_IdlTot_C")
val BMWtchsvc_tq_ResDec_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsvc_tq_ResDec_C")
val BMWtchsvc_tq_ResInc_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsvc_tq_ResInc_C")
val BMWtchsvc_tqi_AirTo_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsvc_tqi_AirTo_C")
val BMWtchsvc_tqi_Air_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchsvc_tqi_Air_T")
val BMWchas_st_OpmAcv_ub: InMeasurement = a2lBin.measurement("BMWchas_st_OpmAcv_ub")
val BMWeisy_p_PreThrPlau_uw: InMeasurement = a2lBin.measurement("BMWeisy_p_PreThrPlau_uw")
val BMWtchsvc_b_Ena_bo: InMeasurement = a2lBin.measurement("BMWtchsvc_b_Ena_bo")
val BMWtchsvc_b_Req_bo: InMeasurement = a2lBin.measurement("BMWtchsvc_b_Req_bo")
val Md_reib_vm: InMeasurement = a2lBin.measurement("Md_reib_vm")
val Mdi_opt_lam: InMeasurement = a2lBin.measurement("Mdi_opt_lam")
val Mdkw_soll: InMeasurement = a2lBin.measurement("Mdkw_soll")
val Nkw: InMeasurement = a2lBin.measurement("Nkw")
val Pumg: InMeasurement = a2lBin.measurement("Pumg")
val Tans: InMeasurement = a2lBin.measurement("Tans")
val Tumg: InMeasurement = a2lBin.measurement("Tumg")
val BMWtchsvc_b_Acv_bo: OutMeasurement = a2lBin.measurement("BMWtchsvc_b_Acv_bo")
val BMWtchsvc_b_CthReq_bo: OutMeasurement = a2lBin.measurement("BMWtchsvc_b_CthReq_bo")
val BMWtchsvc_lam_Req_uw: OutMeasurement = a2lBin.measurement("BMWtchsvc_lam_Req_uw")
val BMWtchsvc_n_Idl_sw: OutMeasurement = a2lBin.measurement("BMWtchsvc_n_Idl_sw")
val BMWtchsvc_p_Delt_uw: OutMeasurement = a2lBin.measurement("BMWtchsvc_p_Delt_uw")
val BMWtchsvc_pct_Wg_uw: OutMeasurement = a2lBin.measurement("BMWtchsvc_pct_Wg_uw")
val BMWtchsvc_st_Tstr_ub: OutMeasurement = a2lBin.measurement("BMWtchsvc_st_Tstr_ub")
val BMWtchsvc_tq_Res_sw: OutMeasurement = a2lBin.measurement("BMWtchsvc_tq_Res_sw")

  BMW_MOD_TchSvc_Acv_100ms(BMWchas_st_OpmAcv_ub, BMWeisy_p_PreThrPlau_uw, BMWtchsvc_b_Ena_bo, BMWtchsvc_b_Req_bo, Md_reib_vm, Mdi_opt_lam, Mdkw_soll, Nkw, Pumg, Tans, Tumg, BMWtchsvc_fac_FilP_C, BMWtchsvc_fac_p_Crtn_M, BMWtchsvc_lam_Req_C, BMWtchsvc_n_IdlDec_C, BMWtchsvc_n_IdlInc_C, BMWtchsvc_n_IdlMax_C, BMWtchsvc_n_IdlMin_C, BMWtchsvc_n_Idl_C, BMWtchsvc_p_DeltMin_T, BMWtchsvc_pct_WgOp_C, BMWtchsvc_ti_CthAcvMax_C, BMWtchsvc_ti_DebIdlNotOk_C, BMWtchsvc_ti_DebPOk_C, BMWtchsvc_ti_DebTqNotOk_C, BMWtchsvc_ti_PChkMax_C, BMWtchsvc_ti_TqMax_C, BMWtchsvc_ti_n_IdlTot_C, BMWtchsvc_tq_ResDec_C, BMWtchsvc_tq_ResInc_C, BMWtchsvc_tqi_AirTo_C, BMWtchsvc_tqi_Air_T, BMWtchsvc_b_Acv_bo, BMWtchsvc_b_CthReq_bo, BMWtchsvc_lam_Req_uw, BMWtchsvc_n_Idl_sw, BMWtchsvc_p_Delt_uw, BMWtchsvc_pct_Wg_uw, BMWtchsvc_st_Tstr_ub, BMWtchsvc_tq_Res_sw)
}


def BMW_MOD_TchSvc_Ena_100ms(BMWbdy_b_CluOp10_bo: InMeasurement, Gangi: InMeasurement, Pumg: InMeasurement, Pwg_ist: InMeasurement, St_atldiag2_in: InMeasurement, St_atldiag2_in_B_atlsvc_if: InMeasurement, St_getriebe: InMeasurement, St_getriebe_B_autget: InMeasurement, St_getriebe_B_dkgget: InMeasurement, St_llr_hyb: InMeasurement, St_llr_hyb_B_em1_max_xctl: InMeasurement, St_llr_hyb_B_em1_nctl_akt: InMeasurement, St_llr_hyb_B_em1_nctl_akt_k0zu: InMeasurement, St_llr_hyb_B_em1_nctl_akt_os: InMeasurement, St_mdllr_swi: InMeasurement, St_mdllr_swi_B_llr_on: InMeasurement, Tmot: InMeasurement, V: InMeasurement, BMWtchsvc_p_AmbMin_C: BigDecimal, BMWtchsvc_swi_Req_C: String, BMWtchsvc_t_EngMax_C: BigDecimal, BMWtchsvc_t_EngMin_C: BigDecimal, BMW_PT_TOP_PX_C: String, BMWtchsvc_b_Ena_bo: OutMeasurement, BMWtchsvc_b_Req_bo: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchSvc_Ena_100ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchsvc_p_AmbMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsvc_p_AmbMin_C")
val BMWtchsvc_swi_Req_C: String = a2lBin.readCharacteristicWithCast("BMWtchsvc_swi_Req_C")
val BMWtchsvc_t_EngMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsvc_t_EngMax_C")
val BMWtchsvc_t_EngMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchsvc_t_EngMin_C")
val BMW_PT_TOP_PX_C: String = a2lBin.readCharacteristicWithCast("BMW_PT_TOP_PX_C")
val BMWbdy_b_CluOp10_bo: InMeasurement = a2lBin.measurement("BMWbdy_b_CluOp10_bo")
val Gangi: InMeasurement = a2lBin.measurement("Gangi")
val Pumg: InMeasurement = a2lBin.measurement("Pumg")
val Pwg_ist: InMeasurement = a2lBin.measurement("Pwg_ist")
val St_atldiag2_in: InMeasurement = a2lBin.measurement("St_atldiag2_in")
val St_atldiag2_in_B_atlsvc_if: InMeasurement = a2lBin.measurement("St_atldiag2_in.B_atlsvc_if")
val St_getriebe: InMeasurement = a2lBin.measurement("St_getriebe")
val St_getriebe_B_autget: InMeasurement = a2lBin.measurement("St_getriebe.B_autget")
val St_getriebe_B_dkgget: InMeasurement = a2lBin.measurement("St_getriebe.B_dkgget")
val St_llr_hyb: InMeasurement = a2lBin.measurement("St_llr_hyb")
val St_llr_hyb_B_em1_max_xctl: InMeasurement = a2lBin.measurement("St_llr_hyb.B_em1_max_xctl")
val St_llr_hyb_B_em1_nctl_akt: InMeasurement = a2lBin.measurement("St_llr_hyb.B_em1_nctl_akt")
val St_llr_hyb_B_em1_nctl_akt_k0zu: InMeasurement = a2lBin.measurement("St_llr_hyb.B_em1_nctl_akt_k0zu")
val St_llr_hyb_B_em1_nctl_akt_os: InMeasurement = a2lBin.measurement("St_llr_hyb.B_em1_nctl_akt_os")
val St_mdllr_swi: InMeasurement = a2lBin.measurement("St_mdllr_swi")
val St_mdllr_swi_B_llr_on: InMeasurement = a2lBin.measurement("St_mdllr_swi.B_llr_on")
val Tmot: InMeasurement = a2lBin.measurement("Tmot")
val V: InMeasurement = a2lBin.measurement("V")
val BMWtchsvc_b_Ena_bo: OutMeasurement = a2lBin.measurement("BMWtchsvc_b_Ena_bo")
val BMWtchsvc_b_Req_bo: OutMeasurement = a2lBin.measurement("BMWtchsvc_b_Req_bo")

  BMW_MOD_TchSvc_Ena_100ms(BMWbdy_b_CluOp10_bo, Gangi, Pumg, Pwg_ist, St_atldiag2_in, St_atldiag2_in_B_atlsvc_if, St_getriebe, St_getriebe_B_autget, St_getriebe_B_dkgget, St_llr_hyb, St_llr_hyb_B_em1_max_xctl, St_llr_hyb_B_em1_nctl_akt, St_llr_hyb_B_em1_nctl_akt_k0zu, St_llr_hyb_B_em1_nctl_akt_os, St_mdllr_swi, St_mdllr_swi_B_llr_on, Tmot, V, BMWtchsvc_p_AmbMin_C, BMWtchsvc_swi_Req_C, BMWtchsvc_t_EngMax_C, BMWtchsvc_t_EngMin_C, BMW_PT_TOP_PX_C, BMWtchsvc_b_Ena_bo, BMWtchsvc_b_Req_bo)
}


def BMW_MOD_TchTbc_Co_100ms(BMWlhmplau_t_EnvUnf_sw: InMeasurement, BMWtchtbc_t_EngAmb_sw: InMeasurement, Msdk: InMeasurement, Ta_agd: InMeasurement, Tans: InMeasurement, Tumg: InMeasurement, BMWtchtbc_cw_t_BefCmpr_C: BigDecimal, BMWtchtbc_fac_FilFade_C: BigDecimal, BMWtchtbc_fac_t_Fade_T: CurveType[BigDecimal, BigDecimal], BMWtchtbc_swi_t_Amb_C: String, BMWtchtbc_t_BefCmprSub_C: BigDecimal, BMWtchtbc_t_Norm_C: BigDecimal, BMWtchtbc_fac_t_BefComp_uw: OutMeasurement, BMWtchtbc_t_BefCmprMnfAvg_sw: OutMeasurement, BMWtchtbc_t_BefCmpr_sw: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchTbc_Co_100ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchtbc_cw_t_BefCmpr_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchtbc_cw_t_BefCmpr_C")
val BMWtchtbc_fac_FilFade_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchtbc_fac_FilFade_C")
val BMWtchtbc_fac_t_Fade_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchtbc_fac_t_Fade_T")
val BMWtchtbc_swi_t_Amb_C: String = a2lBin.readCharacteristicWithCast("BMWtchtbc_swi_t_Amb_C")
val BMWtchtbc_t_BefCmprSub_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchtbc_t_BefCmprSub_C")
val BMWtchtbc_t_Norm_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchtbc_t_Norm_C")
val BMWlhmplau_t_EnvUnf_sw: InMeasurement = a2lBin.measurement("BMWlhmplau_t_EnvUnf_sw")
val BMWtchtbc_t_EngAmb_sw: InMeasurement = a2lBin.measurement("BMWtchtbc_t_EngAmb_sw")
val Msdk: InMeasurement = a2lBin.measurement("Msdk")
val Ta_agd: InMeasurement = a2lBin.measurement("Ta_agd")
val Tans: InMeasurement = a2lBin.measurement("Tans")
val Tumg: InMeasurement = a2lBin.measurement("Tumg")
val BMWtchtbc_fac_t_BefComp_uw: OutMeasurement = a2lBin.measurement("BMWtchtbc_fac_t_BefComp_uw")
val BMWtchtbc_t_BefCmprMnfAvg_sw: OutMeasurement = a2lBin.measurement("BMWtchtbc_t_BefCmprMnfAvg_sw")
val BMWtchtbc_t_BefCmpr_sw: OutMeasurement = a2lBin.measurement("BMWtchtbc_t_BefCmpr_sw")

  BMW_MOD_TchTbc_Co_100ms(BMWlhmplau_t_EnvUnf_sw, BMWtchtbc_t_EngAmb_sw, Msdk, Ta_agd, Tans, Tumg, BMWtchtbc_cw_t_BefCmpr_C, BMWtchtbc_fac_FilFade_C, BMWtchtbc_fac_t_Fade_T, BMWtchtbc_swi_t_Amb_C, BMWtchtbc_t_BefCmprSub_C, BMWtchtbc_t_Norm_C, BMWtchtbc_fac_t_BefComp_uw, BMWtchtbc_t_BefCmprMnfAvg_sw, BMWtchtbc_t_BefCmpr_sw)
}


def BMW_MOD_TchTbc_Mdl_100ms(BMWeng_b_CkMovg_bo: InMeasurement, BMWeng_b_StrEnd_bo: InMeasurement, Mszyl: InMeasurement, Nelueft_wm: InMeasurement, St_kl15: InMeasurement, St_kl15_B_dc_new: InMeasurement, St_kl15_B_kl15_ep: InMeasurement, St_kl15_B_kl15_vorab: InMeasurement, Tabg_mw: InMeasurement, Tmot: InMeasurement, Tn_abstell: InMeasurement, Tumg: InMeasurement, V_can: InMeasurement, BMWtchtbc_fac_EngHotIni_T: CurveType[BigDecimal, BigDecimal], BMWtchtbc_fac_EngHot_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchtbc_fac_FadeWoLoad_T: CurveType[BigDecimal, BigDecimal], BMWtchtbc_fac_FilFadeWoLoad_T: CurveType[BigDecimal, BigDecimal], BMWtchtbc_fac_FilTEx_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchtbc_fac_FilVVeh_T: CurveType[BigDecimal, BigDecimal], BMWtchtbc_fac_TExIni_T: CurveType[BigDecimal, BigDecimal], BMWtchtbc_fac_ti_EngStop_T: CurveType[BigDecimal, BigDecimal], BMWtchtbc_swi_EngAmbIniOld_C: String, BMWtchtbc_t_ExDifMax_C: BigDecimal, BMWtchtbc_t_ExDifMin_C: BigDecimal, BMWtchtbc_t_ExStat_T: CurveType[BigDecimal, BigDecimal], BMWtchtbc_ti_EngStopMax_C: BigDecimal, BMWtchtbc_v_Corr_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtchtbc_fac_EngHot_ub: OutMeasurement, BMWtchtbc_mair_Acv_uw: OutMeasurement, BMWtchtbc_mair_Save_uw: OutMeasurement, BMWtchtbc_t_EngAmbWiLoadSave_sw: OutMeasurement, BMWtchtbc_t_EngAmb_sw: OutMeasurement, BMWtchtbc_ti_EngStopSave_uw: OutMeasurement, BMWtchtbc_v_VehEfc_uw: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchTbc_Mdl_100ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtchtbc_fac_EngHotIni_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchtbc_fac_EngHotIni_T")
val BMWtchtbc_fac_EngHot_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchtbc_fac_EngHot_M")
val BMWtchtbc_fac_FadeWoLoad_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchtbc_fac_FadeWoLoad_T")
val BMWtchtbc_fac_FilFadeWoLoad_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchtbc_fac_FilFadeWoLoad_T")
val BMWtchtbc_fac_FilTEx_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchtbc_fac_FilTEx_M")
val BMWtchtbc_fac_FilVVeh_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchtbc_fac_FilVVeh_T")
val BMWtchtbc_fac_TExIni_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchtbc_fac_TExIni_T")
val BMWtchtbc_fac_ti_EngStop_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchtbc_fac_ti_EngStop_T")
val BMWtchtbc_swi_EngAmbIniOld_C: String = a2lBin.readCharacteristicWithCast("BMWtchtbc_swi_EngAmbIniOld_C")
val BMWtchtbc_t_ExDifMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchtbc_t_ExDifMax_C")
val BMWtchtbc_t_ExDifMin_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchtbc_t_ExDifMin_C")
val BMWtchtbc_t_ExStat_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchtbc_t_ExStat_T")
val BMWtchtbc_ti_EngStopMax_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtchtbc_ti_EngStopMax_C")
val BMWtchtbc_v_Corr_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtchtbc_v_Corr_M")
val BMWeng_b_CkMovg_bo: InMeasurement = a2lBin.measurement("BMWeng_b_CkMovg_bo")
val BMWeng_b_StrEnd_bo: InMeasurement = a2lBin.measurement("BMWeng_b_StrEnd_bo")
val Mszyl: InMeasurement = a2lBin.measurement("Mszyl")
val Nelueft_wm: InMeasurement = a2lBin.measurement("Nelueft_wm")
val St_kl15: InMeasurement = a2lBin.measurement("St_kl15")
val St_kl15_B_dc_new: InMeasurement = a2lBin.measurement("St_kl15.B_dc_new")
val St_kl15_B_kl15_ep: InMeasurement = a2lBin.measurement("St_kl15.B_kl15_ep")
val St_kl15_B_kl15_vorab: InMeasurement = a2lBin.measurement("St_kl15.B_kl15_vorab")
val Tabg_mw: InMeasurement = a2lBin.measurement("Tabg_mw")
val Tmot: InMeasurement = a2lBin.measurement("Tmot")
val Tn_abstell: InMeasurement = a2lBin.measurement("Tn_abstell")
val Tumg: InMeasurement = a2lBin.measurement("Tumg")
val V_can: InMeasurement = a2lBin.measurement("V_can")
val BMWtchtbc_fac_EngHot_ub: OutMeasurement = a2lBin.measurement("BMWtchtbc_fac_EngHot_ub")
val BMWtchtbc_mair_Acv_uw: OutMeasurement = a2lBin.measurement("BMWtchtbc_mair_Acv_uw")
val BMWtchtbc_mair_Save_uw: OutMeasurement = a2lBin.measurement("BMWtchtbc_mair_Save_uw")
val BMWtchtbc_t_EngAmbWiLoadSave_sw: OutMeasurement = a2lBin.measurement("BMWtchtbc_t_EngAmbWiLoadSave_sw")
val BMWtchtbc_t_EngAmb_sw: OutMeasurement = a2lBin.measurement("BMWtchtbc_t_EngAmb_sw")
val BMWtchtbc_ti_EngStopSave_uw: OutMeasurement = a2lBin.measurement("BMWtchtbc_ti_EngStopSave_uw")
val BMWtchtbc_v_VehEfc_uw: OutMeasurement = a2lBin.measurement("BMWtchtbc_v_VehEfc_uw")

  BMW_MOD_TchTbc_Mdl_100ms(BMWeng_b_CkMovg_bo, BMWeng_b_StrEnd_bo, Mszyl, Nelueft_wm, St_kl15, St_kl15_B_dc_new, St_kl15_B_kl15_ep, St_kl15_B_kl15_vorab, Tabg_mw, Tmot, Tn_abstell, Tumg, V_can, BMWtchtbc_fac_EngHotIni_T, BMWtchtbc_fac_EngHot_M, BMWtchtbc_fac_FadeWoLoad_T, BMWtchtbc_fac_FilFadeWoLoad_T, BMWtchtbc_fac_FilTEx_M, BMWtchtbc_fac_FilVVeh_T, BMWtchtbc_fac_TExIni_T, BMWtchtbc_fac_ti_EngStop_T, BMWtchtbc_swi_EngAmbIniOld_C, BMWtchtbc_t_ExDifMax_C, BMWtchtbc_t_ExDifMin_C, BMWtchtbc_t_ExStat_T, BMWtchtbc_ti_EngStopMax_C, BMWtchtbc_v_Corr_M, BMWtchtbc_fac_EngHot_ub, BMWtchtbc_mair_Acv_uw, BMWtchtbc_mair_Save_uw, BMWtchtbc_t_EngAmbWiLoadSave_sw, BMWtchtbc_t_EngAmb_sw, BMWtchtbc_ti_EngStopSave_uw, BMWtchtbc_v_VehEfc_uw)
}


def BMW_MOD_TchTbc_Mdl_swini(BMWeng_b_EcuPwsFaild_bo: InMeasurement, BMWtchtbc_mair_Save_uw: InMeasurement, BMWtchtbc_t_EngAmbWiLoadSave_sw: InMeasurement, BMWtchtbc_ti_EngStopSave_uw: InMeasurement): Unit = {
 ???
}

def BMW_MOD_TchTbc_Mdl_swini(a2lBin: A2LBinAdapter): Unit = {


val BMWeng_b_EcuPwsFaild_bo: InMeasurement = a2lBin.measurement("BMWeng_b_EcuPwsFaild_bo")
val BMWtchtbc_mair_Save_uw: InMeasurement = a2lBin.measurement("BMWtchtbc_mair_Save_uw")
val BMWtchtbc_t_EngAmbWiLoadSave_sw: InMeasurement = a2lBin.measurement("BMWtchtbc_t_EngAmbWiLoadSave_sw")
val BMWtchtbc_ti_EngStopSave_uw: InMeasurement = a2lBin.measurement("BMWtchtbc_ti_EngStopSave_uw")


  BMW_MOD_TchTbc_Mdl_swini(BMWeng_b_EcuPwsFaild_bo, BMWtchtbc_mair_Save_uw, BMWtchtbc_t_EngAmbWiLoadSave_sw, BMWtchtbc_ti_EngStopSave_uw)
}


def BMW_MOD_TchWrap_100ms(BMWtchdiag_b_Acv_bo: InMeasurement, BMWtchdiag_b_RbmDen_bo: InMeasurement, BMWtchdiag_b_RbmNum_bo: InMeasurement, BMWtchsvc_p_Delt_uw: InMeasurement, BMWtchsvc_st_Tstr_ub: InMeasurement, BMWtchtbc_mair_Acv_uw: InMeasurement, BMWtchtbc_mair_Save_uw: InMeasurement, BMWtchtbc_t_BefCmpr_sw: InMeasurement, BMWtchtbc_t_EngAmb_sw: InMeasurement, BMWtchtbc_v_VehEfc_uw: InMeasurement, Atlsvc_dpvdk1: OutMeasurement, Mslui_lang: OutMeasurement, Mslui_lang_nv: OutMeasurement, St_atldiag_10ms: OutMeasurement, St_atldiag_10ms_B_beratllek: OutMeasurement, St_atldiag_rbm: OutMeasurement, St_atldiag_rbm_B_atlr_denom: OutMeasurement, St_atldiag_rbm_B_atlr_nom_b1: OutMeasurement, St_atlsvc: OutMeasurement, St_atlsvc_pvdk: OutMeasurement, Tmotraum: OutMeasurement, Tvldr: OutMeasurement, V_tmotraum_kor: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchWrap_100ms(a2lBin: A2LBinAdapter): Unit = {


val BMWtchdiag_b_Acv_bo: InMeasurement = a2lBin.measurement("BMWtchdiag_b_Acv_bo")
val BMWtchdiag_b_RbmDen_bo: InMeasurement = a2lBin.measurement("BMWtchdiag_b_RbmDen_bo")
val BMWtchdiag_b_RbmNum_bo: InMeasurement = a2lBin.measurement("BMWtchdiag_b_RbmNum_bo")
val BMWtchsvc_p_Delt_uw: InMeasurement = a2lBin.measurement("BMWtchsvc_p_Delt_uw")
val BMWtchsvc_st_Tstr_ub: InMeasurement = a2lBin.measurement("BMWtchsvc_st_Tstr_ub")
val BMWtchtbc_mair_Acv_uw: InMeasurement = a2lBin.measurement("BMWtchtbc_mair_Acv_uw")
val BMWtchtbc_mair_Save_uw: InMeasurement = a2lBin.measurement("BMWtchtbc_mair_Save_uw")
val BMWtchtbc_t_BefCmpr_sw: InMeasurement = a2lBin.measurement("BMWtchtbc_t_BefCmpr_sw")
val BMWtchtbc_t_EngAmb_sw: InMeasurement = a2lBin.measurement("BMWtchtbc_t_EngAmb_sw")
val BMWtchtbc_v_VehEfc_uw: InMeasurement = a2lBin.measurement("BMWtchtbc_v_VehEfc_uw")
val Atlsvc_dpvdk1: OutMeasurement = a2lBin.measurement("Atlsvc_dpvdk1")
val Mslui_lang: OutMeasurement = a2lBin.measurement("Mslui_lang")
val Mslui_lang_nv: OutMeasurement = a2lBin.measurement("Mslui_lang_nv")
val St_atldiag_10ms: OutMeasurement = a2lBin.measurement("St_atldiag_10ms")
val St_atldiag_10ms_B_beratllek: OutMeasurement = a2lBin.measurement("St_atldiag_10ms.B_beratllek")
val St_atldiag_rbm: OutMeasurement = a2lBin.measurement("St_atldiag_rbm")
val St_atldiag_rbm_B_atlr_denom: OutMeasurement = a2lBin.measurement("St_atldiag_rbm.B_atlr_denom")
val St_atldiag_rbm_B_atlr_nom_b1: OutMeasurement = a2lBin.measurement("St_atldiag_rbm.B_atlr_nom_b1")
val St_atlsvc: OutMeasurement = a2lBin.measurement("St_atlsvc")
val St_atlsvc_pvdk: OutMeasurement = a2lBin.measurement("St_atlsvc_pvdk")
val Tmotraum: OutMeasurement = a2lBin.measurement("Tmotraum")
val Tvldr: OutMeasurement = a2lBin.measurement("Tvldr")
val V_tmotraum_kor: OutMeasurement = a2lBin.measurement("V_tmotraum_kor")

  BMW_MOD_TchWrap_100ms(BMWtchdiag_b_Acv_bo, BMWtchdiag_b_RbmDen_bo, BMWtchdiag_b_RbmNum_bo, BMWtchsvc_p_Delt_uw, BMWtchsvc_st_Tstr_ub, BMWtchtbc_mair_Acv_uw, BMWtchtbc_mair_Save_uw, BMWtchtbc_t_BefCmpr_sw, BMWtchtbc_t_EngAmb_sw, BMWtchtbc_v_VehEfc_uw, Atlsvc_dpvdk1, Mslui_lang, Mslui_lang_nv, St_atldiag_10ms, St_atldiag_10ms_B_beratllek, St_atldiag_rbm, St_atldiag_rbm_B_atlr_denom, St_atldiag_rbm_B_atlr_nom_b1, St_atlsvc, St_atlsvc_pvdk, Tmotraum, Tvldr, V_tmotraum_kor)
}


def BMW_MOD_TchWrap_10ms(BMWtchbas_p_Dif_sw: InMeasurement, BMWtchbov_b_Acv_bo: InMeasurement, BMWtchbov_mf_ThrMin_uw: InMeasurement, BMWtchbov_mf_VvtMin_uw: InMeasurement, BMWtchob_tq_Delt_sw: InMeasurement, BMWtchout_f_GasWg_uw: InMeasurement, BMWtchout_pct_Wg_uw: InMeasurement, BMWtchscv_b_Req_bo: InMeasurement, BMWtchscv_fac__ub: InMeasurement, BMWtchsp_mf_Ex_uw: InMeasurement, BMWtchsp_rat_p_CmprLim_uw: InMeasurement, Dmd_over_boost: OutMeasurement, F_atldyn: OutMeasurement, F_pldsvld_f: OutMeasurement, Mssol_akzu: OutMeasurement, Mssolldk_ulv: OutMeasurement, Mssollvt_ulv: OutMeasurement, Pld_diff_xeb: OutMeasurement, St_atlstat: OutMeasurement, St_atlstat_B_atldyn: OutMeasurement, Tvulv: OutMeasurement, Tvwg: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchWrap_10ms(a2lBin: A2LBinAdapter): Unit = {


val BMWtchbas_p_Dif_sw: InMeasurement = a2lBin.measurement("BMWtchbas_p_Dif_sw")
val BMWtchbov_b_Acv_bo: InMeasurement = a2lBin.measurement("BMWtchbov_b_Acv_bo")
val BMWtchbov_mf_ThrMin_uw: InMeasurement = a2lBin.measurement("BMWtchbov_mf_ThrMin_uw")
val BMWtchbov_mf_VvtMin_uw: InMeasurement = a2lBin.measurement("BMWtchbov_mf_VvtMin_uw")
val BMWtchob_tq_Delt_sw: InMeasurement = a2lBin.measurement("BMWtchob_tq_Delt_sw")
val BMWtchout_f_GasWg_uw: InMeasurement = a2lBin.measurement("BMWtchout_f_GasWg_uw")
val BMWtchout_pct_Wg_uw: InMeasurement = a2lBin.measurement("BMWtchout_pct_Wg_uw")
val BMWtchscv_b_Req_bo: InMeasurement = a2lBin.measurement("BMWtchscv_b_Req_bo")
val BMWtchscv_fac__ub: InMeasurement = a2lBin.measurement("BMWtchscv_fac__ub")
val BMWtchsp_mf_Ex_uw: InMeasurement = a2lBin.measurement("BMWtchsp_mf_Ex_uw")
val BMWtchsp_rat_p_CmprLim_uw: InMeasurement = a2lBin.measurement("BMWtchsp_rat_p_CmprLim_uw")
val Dmd_over_boost: OutMeasurement = a2lBin.measurement("Dmd_over_boost")
val F_atldyn: OutMeasurement = a2lBin.measurement("F_atldyn")
val F_pldsvld_f: OutMeasurement = a2lBin.measurement("F_pldsvld_f")
val Mssol_akzu: OutMeasurement = a2lBin.measurement("Mssol_akzu")
val Mssolldk_ulv: OutMeasurement = a2lBin.measurement("Mssolldk_ulv")
val Mssollvt_ulv: OutMeasurement = a2lBin.measurement("Mssollvt_ulv")
val Pld_diff_xeb: OutMeasurement = a2lBin.measurement("Pld_diff_xeb")
val St_atlstat: OutMeasurement = a2lBin.measurement("St_atlstat")
val St_atlstat_B_atldyn: OutMeasurement = a2lBin.measurement("St_atlstat.B_atldyn")
val Tvulv: OutMeasurement = a2lBin.measurement("Tvulv")
val Tvwg: OutMeasurement = a2lBin.measurement("Tvwg")

  BMW_MOD_TchWrap_10ms(BMWtchbas_p_Dif_sw, BMWtchbov_b_Acv_bo, BMWtchbov_mf_ThrMin_uw, BMWtchbov_mf_VvtMin_uw, BMWtchob_tq_Delt_sw, BMWtchout_f_GasWg_uw, BMWtchout_pct_Wg_uw, BMWtchscv_b_Req_bo, BMWtchscv_fac__ub, BMWtchsp_mf_Ex_uw, BMWtchsp_rat_p_CmprLim_uw, Dmd_over_boost, F_atldyn, F_pldsvld_f, Mssol_akzu, Mssolldk_ulv, Mssollvt_ulv, Pld_diff_xeb, St_atlstat, St_atlstat_B_atldyn, Tvulv, Tvwg)
}


def BMW_MOD_TchWrap_swini(Atlsvc_dpvdk2: OutMeasurement, Atlsvc_dpvdk3: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_TchWrap_swini(a2lBin: A2LBinAdapter): Unit = {



val Atlsvc_dpvdk2: OutMeasurement = a2lBin.measurement("Atlsvc_dpvdk2")
val Atlsvc_dpvdk3: OutMeasurement = a2lBin.measurement("Atlsvc_dpvdk3")

  BMW_MOD_TchWrap_swini(Atlsvc_dpvdk2, Atlsvc_dpvdk3)
}

}
