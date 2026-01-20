
package net.jtownson.xdfbinext.bace.DME861_R1C9J8B3B
import net.jtownson.xdfbinext.bace.BaceDSL.*
import net.jtownson.xdfbinext.A2LBinAdapter
import net.jtownson.xdfbinext.a2l.A2LMeasurement.{InMeasurement, OutMeasurement}
import net.jtownson.xdfbinext.a2l.{CurveType, MapType, A2LMeasurement}

object Tqw_111_001_0 {

def BMW_MOD_CluWarn(BMWbdy_b_CluOp10_bo: InMeasurement, BMWbdy_b_CluOp90_bo: InMeasurement, BMWmsa_stb_MsaDrr_ub: InMeasurement, BMWmsa_stb_MsaDrr_ub_B_fahrbereit: InMeasurement, BMWmsa_stb_MsaDrr_ub_B_fahrbereit_pwf: InMeasurement, Gangi: InMeasurement, Mdk_wunsch: InMeasurement, V_can: InMeasurement, Var_hs: InMeasurement, CW_KP_SCHLAF_INAKTIV_GANG: BigDecimal, KF_TD_KP_SCHLAF_ERKENNUNG: MapType[BigDecimal, BigDecimal, BigDecimal], K_KP_SCHLAF_FID_OK: BigDecimal, K_KP_SCHLAF_MAX_WARNUNGEN: BigDecimal, K_KP_SCHLAF_MIN_V: BigDecimal, K_TD_KP_SCHLAF_MINIMAL_VERZ: BigDecimal, BMW_PT_TOP_COENG_C: String): Unit = {
 ???
}

def BMW_MOD_CluWarn(a2lBin: A2LBinAdapter): Unit = {

val CW_KP_SCHLAF_INAKTIV_GANG: BigDecimal = a2lBin.readCharacteristicWithCast("CW_KP_SCHLAF_INAKTIV_GANG")
val KF_TD_KP_SCHLAF_ERKENNUNG: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_TD_KP_SCHLAF_ERKENNUNG")
val K_KP_SCHLAF_FID_OK: BigDecimal = a2lBin.readCharacteristicWithCast("K_KP_SCHLAF_FID_OK")
val K_KP_SCHLAF_MAX_WARNUNGEN: BigDecimal = a2lBin.readCharacteristicWithCast("K_KP_SCHLAF_MAX_WARNUNGEN")
val K_KP_SCHLAF_MIN_V: BigDecimal = a2lBin.readCharacteristicWithCast("K_KP_SCHLAF_MIN_V")
val K_TD_KP_SCHLAF_MINIMAL_VERZ: BigDecimal = a2lBin.readCharacteristicWithCast("K_TD_KP_SCHLAF_MINIMAL_VERZ")
val BMW_PT_TOP_COENG_C: String = a2lBin.readCharacteristicWithCast("BMW_PT_TOP_COENG_C")
val BMWbdy_b_CluOp10_bo: InMeasurement = a2lBin.measurement("BMWbdy_b_CluOp10_bo")
val BMWbdy_b_CluOp90_bo: InMeasurement = a2lBin.measurement("BMWbdy_b_CluOp90_bo")
val BMWmsa_stb_MsaDrr_ub: InMeasurement = a2lBin.measurement("BMWmsa_stb_MsaDrr_ub")
val BMWmsa_stb_MsaDrr_ub_B_fahrbereit: InMeasurement = a2lBin.measurement("BMWmsa_stb_MsaDrr_ub.B_fahrbereit")
val BMWmsa_stb_MsaDrr_ub_B_fahrbereit_pwf: InMeasurement = a2lBin.measurement("BMWmsa_stb_MsaDrr_ub.B_fahrbereit_pwf")
val Gangi: InMeasurement = a2lBin.measurement("Gangi")
val Mdk_wunsch: InMeasurement = a2lBin.measurement("Mdk_wunsch")
val V_can: InMeasurement = a2lBin.measurement("V_can")
val Var_hs: InMeasurement = a2lBin.measurement("Var_hs")


  BMW_MOD_CluWarn(BMWbdy_b_CluOp10_bo, BMWbdy_b_CluOp90_bo, BMWmsa_stb_MsaDrr_ub, BMWmsa_stb_MsaDrr_ub_B_fahrbereit, BMWmsa_stb_MsaDrr_ub_B_fahrbereit_pwf, Gangi, Mdk_wunsch, V_can, Var_hs, CW_KP_SCHLAF_INAKTIV_GANG, KF_TD_KP_SCHLAF_ERKENNUNG, K_KP_SCHLAF_FID_OK, K_KP_SCHLAF_MAX_WARNUNGEN, K_KP_SCHLAF_MIN_V, K_TD_KP_SCHLAF_MINIMAL_VERZ, BMW_PT_TOP_COENG_C)
}


def BMW_MOD_IfMgr_ActTqwExt(BMWtqc_Rat_GbxWhlHaxl: InMeasurement, BMWtqe_st_tq_LimSrc_ul: InMeasurement, BMWtqw_fac_StatLossHa_ub: InMeasurement, BMWtqw_stb_FocDtHa_ub: InMeasurement, BMWtqw_stb_FocDtHa_ub_BMWtqw_b_FocGbxHa_bc: InMeasurement, BMWtqw_tqw_DynLossHa_sw: InMeasurement, BMWtqw_tqw_StatLossHa_sw: InMeasurement, Md_can_dmee: InMeasurement, Md_em1_ist_plaus: InMeasurement, Mdg_ist_Haxl_nlow: InMeasurement, Msa_startsyst: InMeasurement, Nkw_10ms: InMeasurement, Nturb_plaus: InMeasurement, St_gangwechsel_akt: InMeasurement, St_msasiko: InMeasurement, St_msasiko_B_fahrbereit_siko: InMeasurement, St_msasiko_B_msa_fb_erl: InMeasurement, St_msasiko_B_msaakt_siko: InMeasurement, St_msasiko_B_msastart_siko: InMeasurement, St_msasiko_B_msastopp_siko: InMeasurement, St_msasiko_B_msavmkssoll_siko: InMeasurement, St_msasiko_B_msavmstsoll_siko: InMeasurement, St_msasiko_B_sgrhwl_siko: InMeasurement, St_wk_plaus: InMeasurement, Status_antrieb_ist: InMeasurement, T_sample_tqc: InMeasurement, V_fzg_fahrtricht: InMeasurement, BMWtqw_cw_ActTqwExt_C: BigDecimal, BMWtqw_fac_FacTqGbxInpHaAct_T: CurveType[BigDecimal, BigDecimal], BMWtqw_fac_LpfTqGbxInpHaAct_T: CurveType[BigDecimal, BigDecimal], BMWtqw_fac_LpfTqWhlRaAct_C: BigDecimal, BMWtqw_swi_CfgDt2VehFa_C: BigDecimal, BMWtqw_swi_CfgDt2VehRa_C: BigDecimal, CW_FREEZE_MDVMIST: BigDecimal, CW_GANGWECHSEL_MDVMIST: BigDecimal, CW_LIMSRC_MDVMIST: BigDecimal, KL_MDRISTHA_LIM: CurveType[BigDecimal, BigDecimal], K_FK_EM_STARTSYST: BigDecimal, K_FK_EM_STARTSYST_GRDDN: BigDecimal, K_FK_EM_STARTSYST_GRDUP: BigDecimal, K_MD_EM1_IST_BLS_OFF: BigDecimal, K_MD_EM1_IST_BLS_ON: BigDecimal, K_MD_RAD_IST_BLS_OFF: BigDecimal, K_MD_RAD_IST_BLS_ON: BigDecimal, K_ST_EM_STARTSYST: BigDecimal, K_TD_FREEZE_MDVMIST: BigDecimal, K_TD_GANGWECHSEL_MDVMIST: BigDecimal, K_TD_MD_IST_STARTSYST: BigDecimal, K_TD_ST_BLS_REKU: BigDecimal, MD_RAD_IST_C: String, MD_RAD_IST_HA_C: String, MD_RAD_IST_HA_V: BigDecimal, MD_RAD_IST_V: BigDecimal, BMWtqw_tqw_AvHa_sw: OutMeasurement, Md_rad_ist: OutMeasurement, Md_rad_ist_ha: OutMeasurement, St_bls_reku: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_IfMgr_ActTqwExt(a2lBin: A2LBinAdapter): Unit = {

val BMWtqw_cw_ActTqwExt_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_cw_ActTqwExt_C")
val BMWtqw_fac_FacTqGbxInpHaAct_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtqw_fac_FacTqGbxInpHaAct_T")
val BMWtqw_fac_LpfTqGbxInpHaAct_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtqw_fac_LpfTqGbxInpHaAct_T")
val BMWtqw_fac_LpfTqWhlRaAct_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_fac_LpfTqWhlRaAct_C")
val BMWtqw_swi_CfgDt2VehFa_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_swi_CfgDt2VehFa_C")
val BMWtqw_swi_CfgDt2VehRa_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_swi_CfgDt2VehRa_C")
val CW_FREEZE_MDVMIST: BigDecimal = a2lBin.readCharacteristicWithCast("CW_FREEZE_MDVMIST")
val CW_GANGWECHSEL_MDVMIST: BigDecimal = a2lBin.readCharacteristicWithCast("CW_GANGWECHSEL_MDVMIST")
val CW_LIMSRC_MDVMIST: BigDecimal = a2lBin.readCharacteristicWithCast("CW_LIMSRC_MDVMIST")
val KL_MDRISTHA_LIM: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MDRISTHA_LIM")
val K_FK_EM_STARTSYST: BigDecimal = a2lBin.readCharacteristicWithCast("K_FK_EM_STARTSYST")
val K_FK_EM_STARTSYST_GRDDN: BigDecimal = a2lBin.readCharacteristicWithCast("K_FK_EM_STARTSYST_GRDDN")
val K_FK_EM_STARTSYST_GRDUP: BigDecimal = a2lBin.readCharacteristicWithCast("K_FK_EM_STARTSYST_GRDUP")
val K_MD_EM1_IST_BLS_OFF: BigDecimal = a2lBin.readCharacteristicWithCast("K_MD_EM1_IST_BLS_OFF")
val K_MD_EM1_IST_BLS_ON: BigDecimal = a2lBin.readCharacteristicWithCast("K_MD_EM1_IST_BLS_ON")
val K_MD_RAD_IST_BLS_OFF: BigDecimal = a2lBin.readCharacteristicWithCast("K_MD_RAD_IST_BLS_OFF")
val K_MD_RAD_IST_BLS_ON: BigDecimal = a2lBin.readCharacteristicWithCast("K_MD_RAD_IST_BLS_ON")
val K_ST_EM_STARTSYST: BigDecimal = a2lBin.readCharacteristicWithCast("K_ST_EM_STARTSYST")
val K_TD_FREEZE_MDVMIST: BigDecimal = a2lBin.readCharacteristicWithCast("K_TD_FREEZE_MDVMIST")
val K_TD_GANGWECHSEL_MDVMIST: BigDecimal = a2lBin.readCharacteristicWithCast("K_TD_GANGWECHSEL_MDVMIST")
val K_TD_MD_IST_STARTSYST: BigDecimal = a2lBin.readCharacteristicWithCast("K_TD_MD_IST_STARTSYST")
val K_TD_ST_BLS_REKU: BigDecimal = a2lBin.readCharacteristicWithCast("K_TD_ST_BLS_REKU")
val MD_RAD_IST_C: String = a2lBin.readCharacteristicWithCast("MD_RAD_IST_C")
val MD_RAD_IST_HA_C: String = a2lBin.readCharacteristicWithCast("MD_RAD_IST_HA_C")
val MD_RAD_IST_HA_V: BigDecimal = a2lBin.readCharacteristicWithCast("MD_RAD_IST_HA_V")
val MD_RAD_IST_V: BigDecimal = a2lBin.readCharacteristicWithCast("MD_RAD_IST_V")
val BMWtqc_Rat_GbxWhlHaxl: InMeasurement = a2lBin.measurement("BMWtqc_Rat_GbxWhlHaxl")
val BMWtqe_st_tq_LimSrc_ul: InMeasurement = a2lBin.measurement("BMWtqe_st_tq_LimSrc_ul")
val BMWtqw_fac_StatLossHa_ub: InMeasurement = a2lBin.measurement("BMWtqw_fac_StatLossHa_ub")
val BMWtqw_stb_FocDtHa_ub: InMeasurement = a2lBin.measurement("BMWtqw_stb_FocDtHa_ub")
val BMWtqw_stb_FocDtHa_ub_BMWtqw_b_FocGbxHa_bc: InMeasurement = a2lBin.measurement("BMWtqw_stb_FocDtHa_ub.BMWtqw_b_FocGbxHa_bc")
val BMWtqw_tqw_DynLossHa_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_DynLossHa_sw")
val BMWtqw_tqw_StatLossHa_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_StatLossHa_sw")
val Md_can_dmee: InMeasurement = a2lBin.measurement("Md_can_dmee")
val Md_em1_ist_plaus: InMeasurement = a2lBin.measurement("Md_em1_ist_plaus")
val Mdg_ist_Haxl_nlow: InMeasurement = a2lBin.measurement("Mdg_ist_Haxl_nlow")
val Msa_startsyst: InMeasurement = a2lBin.measurement("Msa_startsyst")
val Nkw_10ms: InMeasurement = a2lBin.measurement("Nkw_10ms")
val Nturb_plaus: InMeasurement = a2lBin.measurement("Nturb_plaus")
val St_gangwechsel_akt: InMeasurement = a2lBin.measurement("St_gangwechsel_akt")
val St_msasiko: InMeasurement = a2lBin.measurement("St_msasiko")
val St_msasiko_B_fahrbereit_siko: InMeasurement = a2lBin.measurement("St_msasiko.B_fahrbereit_siko")
val St_msasiko_B_msa_fb_erl: InMeasurement = a2lBin.measurement("St_msasiko.B_msa_fb_erl")
val St_msasiko_B_msaakt_siko: InMeasurement = a2lBin.measurement("St_msasiko.B_msaakt_siko")
val St_msasiko_B_msastart_siko: InMeasurement = a2lBin.measurement("St_msasiko.B_msastart_siko")
val St_msasiko_B_msastopp_siko: InMeasurement = a2lBin.measurement("St_msasiko.B_msastopp_siko")
val St_msasiko_B_msavmkssoll_siko: InMeasurement = a2lBin.measurement("St_msasiko.B_msavmkssoll_siko")
val St_msasiko_B_msavmstsoll_siko: InMeasurement = a2lBin.measurement("St_msasiko.B_msavmstsoll_siko")
val St_msasiko_B_sgrhwl_siko: InMeasurement = a2lBin.measurement("St_msasiko.B_sgrhwl_siko")
val St_wk_plaus: InMeasurement = a2lBin.measurement("St_wk_plaus")
val Status_antrieb_ist: InMeasurement = a2lBin.measurement("Status_antrieb_ist")
val T_sample_tqc: InMeasurement = a2lBin.measurement("T_sample_tqc")
val V_fzg_fahrtricht: InMeasurement = a2lBin.measurement("V_fzg_fahrtricht")
val BMWtqw_tqw_AvHa_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_AvHa_sw")
val Md_rad_ist: OutMeasurement = a2lBin.measurement("Md_rad_ist")
val Md_rad_ist_ha: OutMeasurement = a2lBin.measurement("Md_rad_ist_ha")
val St_bls_reku: OutMeasurement = a2lBin.measurement("St_bls_reku")

  BMW_MOD_IfMgr_ActTqwExt(BMWtqc_Rat_GbxWhlHaxl, BMWtqe_st_tq_LimSrc_ul, BMWtqw_fac_StatLossHa_ub, BMWtqw_stb_FocDtHa_ub, BMWtqw_stb_FocDtHa_ub_BMWtqw_b_FocGbxHa_bc, BMWtqw_tqw_DynLossHa_sw, BMWtqw_tqw_StatLossHa_sw, Md_can_dmee, Md_em1_ist_plaus, Mdg_ist_Haxl_nlow, Msa_startsyst, Nkw_10ms, Nturb_plaus, St_gangwechsel_akt, St_msasiko, St_msasiko_B_fahrbereit_siko, St_msasiko_B_msa_fb_erl, St_msasiko_B_msaakt_siko, St_msasiko_B_msastart_siko, St_msasiko_B_msastopp_siko, St_msasiko_B_msavmkssoll_siko, St_msasiko_B_msavmstsoll_siko, St_msasiko_B_sgrhwl_siko, St_wk_plaus, Status_antrieb_ist, T_sample_tqc, V_fzg_fahrtricht, BMWtqw_cw_ActTqwExt_C, BMWtqw_fac_FacTqGbxInpHaAct_T, BMWtqw_fac_LpfTqGbxInpHaAct_T, BMWtqw_fac_LpfTqWhlRaAct_C, BMWtqw_swi_CfgDt2VehFa_C, BMWtqw_swi_CfgDt2VehRa_C, CW_FREEZE_MDVMIST, CW_GANGWECHSEL_MDVMIST, CW_LIMSRC_MDVMIST, KL_MDRISTHA_LIM, K_FK_EM_STARTSYST, K_FK_EM_STARTSYST_GRDDN, K_FK_EM_STARTSYST_GRDUP, K_MD_EM1_IST_BLS_OFF, K_MD_EM1_IST_BLS_ON, K_MD_RAD_IST_BLS_OFF, K_MD_RAD_IST_BLS_ON, K_ST_EM_STARTSYST, K_TD_FREEZE_MDVMIST, K_TD_GANGWECHSEL_MDVMIST, K_TD_MD_IST_STARTSYST, K_TD_ST_BLS_REKU, MD_RAD_IST_C, MD_RAD_IST_HA_C, MD_RAD_IST_HA_V, MD_RAD_IST_V, BMWtqw_tqw_AvHa_sw, Md_rad_ist, Md_rad_ist_ha, St_bls_reku)
}


def BMW_MOD_IfMgr_DrvrAsscSys(BMWmsa_stb_MsaDrr_ub: InMeasurement, BMWmsa_stb_MsaDrr_ub_B_fahrbereit: InMeasurement, BMWmsa_stb_MsaDrr_ub_B_fahrbereit_pwf: InMeasurement, BMWtqe_b_Dfco_bo: InMeasurement, Nkw: InMeasurement, Nstat: InMeasurement, St_egs: InMeasurement, St_egs_B_csn_egs: InMeasurement, St_egs_B_egs_khl1: InMeasurement, St_egs_B_egs_khl2: InMeasurement, St_egs_B_nic_egs: InMeasurement, St_fw: InMeasurement, St_fw_B_hschalt_komb: InMeasurement, St_fw_B_ldm_akt04: InMeasurement, St_fw_B_ldm_off: InMeasurement, St_fw_B_ldm_offhs: InMeasurement, St_fw_B_ldm_offreg: InMeasurement, St_fw_B_ldm_offrs: InMeasurement, St_fw_B_rschalt_komb: InMeasurement, St_mdrdmk: InMeasurement, St_mdrdmk_B_fas_dcc: InMeasurement, St_mdrdmk_B_fas_sld: InMeasurement, St_spa3: InMeasurement, St_spa3_B_fgrspa_akt: InMeasurement, St_spa3_B_gangi_begr: InMeasurement, St_spa3_B_gearbox_spa: InMeasurement, St_spa3_B_hschalt: InMeasurement, St_spa3_B_prschalt_ldm: InMeasurement, St_spa3_B_rschalt: InMeasurement, St_spa3_B_rschalt_ldm: InMeasurement, St_spa3_B_td_pwgmax: InMeasurement, CW_IFMGR_DRVRASSCSYS_01: BigDecimal, K_NMIN_LDMAUS: BigDecimal, K_TD_LDMAUS: BigDecimal, K_TD_LDMAUS1: BigDecimal, ST_FAS_MRADIST_C: String, ST_FAS_MRADIST_V: BigDecimal, ST_SOLL_GRB_FAS_V: BigDecimal, ST_SOLL_PRND_GRB_FAS_V: BigDecimal, S_GETRIEBE_ZU_HEISS: String, S_LDMAUSHS_AKT: String, S_LDMAUSRS_AKT: String, St_fas_mradist: OutMeasurement, St_soll_grb_fas: OutMeasurement, St_soll_prnd_grb_fas: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_IfMgr_DrvrAsscSys(a2lBin: A2LBinAdapter): Unit = {

val CW_IFMGR_DRVRASSCSYS_01: BigDecimal = a2lBin.readCharacteristicWithCast("CW_IFMGR_DRVRASSCSYS_01")
val K_NMIN_LDMAUS: BigDecimal = a2lBin.readCharacteristicWithCast("K_NMIN_LDMAUS")
val K_TD_LDMAUS: BigDecimal = a2lBin.readCharacteristicWithCast("K_TD_LDMAUS")
val K_TD_LDMAUS1: BigDecimal = a2lBin.readCharacteristicWithCast("K_TD_LDMAUS1")
val ST_FAS_MRADIST_C: String = a2lBin.readCharacteristicWithCast("ST_FAS_MRADIST_C")
val ST_FAS_MRADIST_V: BigDecimal = a2lBin.readCharacteristicWithCast("ST_FAS_MRADIST_V")
val ST_SOLL_GRB_FAS_V: BigDecimal = a2lBin.readCharacteristicWithCast("ST_SOLL_GRB_FAS_V")
val ST_SOLL_PRND_GRB_FAS_V: BigDecimal = a2lBin.readCharacteristicWithCast("ST_SOLL_PRND_GRB_FAS_V")
val S_GETRIEBE_ZU_HEISS: String = a2lBin.readCharacteristicWithCast("S_GETRIEBE_ZU_HEISS")
val S_LDMAUSHS_AKT: String = a2lBin.readCharacteristicWithCast("S_LDMAUSHS_AKT")
val S_LDMAUSRS_AKT: String = a2lBin.readCharacteristicWithCast("S_LDMAUSRS_AKT")
val BMWmsa_stb_MsaDrr_ub: InMeasurement = a2lBin.measurement("BMWmsa_stb_MsaDrr_ub")
val BMWmsa_stb_MsaDrr_ub_B_fahrbereit: InMeasurement = a2lBin.measurement("BMWmsa_stb_MsaDrr_ub.B_fahrbereit")
val BMWmsa_stb_MsaDrr_ub_B_fahrbereit_pwf: InMeasurement = a2lBin.measurement("BMWmsa_stb_MsaDrr_ub.B_fahrbereit_pwf")
val BMWtqe_b_Dfco_bo: InMeasurement = a2lBin.measurement("BMWtqe_b_Dfco_bo")
val Nkw: InMeasurement = a2lBin.measurement("Nkw")
val Nstat: InMeasurement = a2lBin.measurement("Nstat")
val St_egs: InMeasurement = a2lBin.measurement("St_egs")
val St_egs_B_csn_egs: InMeasurement = a2lBin.measurement("St_egs.B_csn_egs")
val St_egs_B_egs_khl1: InMeasurement = a2lBin.measurement("St_egs.B_egs_khl1")
val St_egs_B_egs_khl2: InMeasurement = a2lBin.measurement("St_egs.B_egs_khl2")
val St_egs_B_nic_egs: InMeasurement = a2lBin.measurement("St_egs.B_nic_egs")
val St_fw: InMeasurement = a2lBin.measurement("St_fw")
val St_fw_B_hschalt_komb: InMeasurement = a2lBin.measurement("St_fw.B_hschalt_komb")
val St_fw_B_ldm_akt04: InMeasurement = a2lBin.measurement("St_fw.B_ldm_akt04")
val St_fw_B_ldm_off: InMeasurement = a2lBin.measurement("St_fw.B_ldm_off")
val St_fw_B_ldm_offhs: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offhs")
val St_fw_B_ldm_offreg: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offreg")
val St_fw_B_ldm_offrs: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offrs")
val St_fw_B_rschalt_komb: InMeasurement = a2lBin.measurement("St_fw.B_rschalt_komb")
val St_mdrdmk: InMeasurement = a2lBin.measurement("St_mdrdmk")
val St_mdrdmk_B_fas_dcc: InMeasurement = a2lBin.measurement("St_mdrdmk.B_fas_dcc")
val St_mdrdmk_B_fas_sld: InMeasurement = a2lBin.measurement("St_mdrdmk.B_fas_sld")
val St_spa3: InMeasurement = a2lBin.measurement("St_spa3")
val St_spa3_B_fgrspa_akt: InMeasurement = a2lBin.measurement("St_spa3.B_fgrspa_akt")
val St_spa3_B_gangi_begr: InMeasurement = a2lBin.measurement("St_spa3.B_gangi_begr")
val St_spa3_B_gearbox_spa: InMeasurement = a2lBin.measurement("St_spa3.B_gearbox_spa")
val St_spa3_B_hschalt: InMeasurement = a2lBin.measurement("St_spa3.B_hschalt")
val St_spa3_B_prschalt_ldm: InMeasurement = a2lBin.measurement("St_spa3.B_prschalt_ldm")
val St_spa3_B_rschalt: InMeasurement = a2lBin.measurement("St_spa3.B_rschalt")
val St_spa3_B_rschalt_ldm: InMeasurement = a2lBin.measurement("St_spa3.B_rschalt_ldm")
val St_spa3_B_td_pwgmax: InMeasurement = a2lBin.measurement("St_spa3.B_td_pwgmax")
val St_fas_mradist: OutMeasurement = a2lBin.measurement("St_fas_mradist")
val St_soll_grb_fas: OutMeasurement = a2lBin.measurement("St_soll_grb_fas")
val St_soll_prnd_grb_fas: OutMeasurement = a2lBin.measurement("St_soll_prnd_grb_fas")

  BMW_MOD_IfMgr_DrvrAsscSys(BMWmsa_stb_MsaDrr_ub, BMWmsa_stb_MsaDrr_ub_B_fahrbereit, BMWmsa_stb_MsaDrr_ub_B_fahrbereit_pwf, BMWtqe_b_Dfco_bo, Nkw, Nstat, St_egs, St_egs_B_csn_egs, St_egs_B_egs_khl1, St_egs_B_egs_khl2, St_egs_B_nic_egs, St_fw, St_fw_B_hschalt_komb, St_fw_B_ldm_akt04, St_fw_B_ldm_off, St_fw_B_ldm_offhs, St_fw_B_ldm_offreg, St_fw_B_ldm_offrs, St_fw_B_rschalt_komb, St_mdrdmk, St_mdrdmk_B_fas_dcc, St_mdrdmk_B_fas_sld, St_spa3, St_spa3_B_fgrspa_akt, St_spa3_B_gangi_begr, St_spa3_B_gearbox_spa, St_spa3_B_hschalt, St_spa3_B_prschalt_ldm, St_spa3_B_rschalt, St_spa3_B_rschalt_ldm, St_spa3_B_td_pwgmax, CW_IFMGR_DRVRASSCSYS_01, K_NMIN_LDMAUS, K_TD_LDMAUS, K_TD_LDMAUS1, ST_FAS_MRADIST_C, ST_FAS_MRADIST_V, ST_SOLL_GRB_FAS_V, ST_SOLL_PRND_GRB_FAS_V, S_GETRIEBE_ZU_HEISS, S_LDMAUSHS_AKT, S_LDMAUSRS_AKT, St_fas_mradist, St_soll_grb_fas, St_soll_prnd_grb_fas)
}


def BMW_MOD_IfMgr_ErrAmp(BMWtqw_tqw_AvHa_sw: InMeasurement, St_getrdaten: InMeasurement, St_getrdaten_B_gangwechsel_gs: InMeasurement, St_wk_plaus: InMeasurement, CW_IFMGR_ERRAMP_01: BigDecimal, ERRAMPFACT_C: String, ERRAMPFACT_V: BigDecimal, KL_F_ST_WK_ERR_AMP: CurveType[BigDecimal, BigDecimal], K_GS_GB1_ERR_AMP: BigDecimal, K_MDRAD_MAX_ERRAMP: BigDecimal, K_MDRAD_MIN_ERRAMP: BigDecimal, K_ST_WK_VAL: BigDecimal, MD_RAD_ERRAMP_C: String, MD_RAD_ERRAMP_V: BigDecimal, Md_rad_erramp: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_IfMgr_ErrAmp(a2lBin: A2LBinAdapter): Unit = {

val CW_IFMGR_ERRAMP_01: BigDecimal = a2lBin.readCharacteristicWithCast("CW_IFMGR_ERRAMP_01")
val ERRAMPFACT_C: String = a2lBin.readCharacteristicWithCast("ERRAMPFACT_C")
val ERRAMPFACT_V: BigDecimal = a2lBin.readCharacteristicWithCast("ERRAMPFACT_V")
val KL_F_ST_WK_ERR_AMP: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_F_ST_WK_ERR_AMP")
val K_GS_GB1_ERR_AMP: BigDecimal = a2lBin.readCharacteristicWithCast("K_GS_GB1_ERR_AMP")
val K_MDRAD_MAX_ERRAMP: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDRAD_MAX_ERRAMP")
val K_MDRAD_MIN_ERRAMP: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDRAD_MIN_ERRAMP")
val K_ST_WK_VAL: BigDecimal = a2lBin.readCharacteristicWithCast("K_ST_WK_VAL")
val MD_RAD_ERRAMP_C: String = a2lBin.readCharacteristicWithCast("MD_RAD_ERRAMP_C")
val MD_RAD_ERRAMP_V: BigDecimal = a2lBin.readCharacteristicWithCast("MD_RAD_ERRAMP_V")
val BMWtqw_tqw_AvHa_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AvHa_sw")
val St_getrdaten: InMeasurement = a2lBin.measurement("St_getrdaten")
val St_getrdaten_B_gangwechsel_gs: InMeasurement = a2lBin.measurement("St_getrdaten.B_gangwechsel_gs")
val St_wk_plaus: InMeasurement = a2lBin.measurement("St_wk_plaus")
val Md_rad_erramp: OutMeasurement = a2lBin.measurement("Md_rad_erramp")

  BMW_MOD_IfMgr_ErrAmp(BMWtqw_tqw_AvHa_sw, St_getrdaten, St_getrdaten_B_gangwechsel_gs, St_wk_plaus, CW_IFMGR_ERRAMP_01, ERRAMPFACT_C, ERRAMPFACT_V, KL_F_ST_WK_ERR_AMP, K_GS_GB1_ERR_AMP, K_MDRAD_MAX_ERRAMP, K_MDRAD_MIN_ERRAMP, K_ST_WK_VAL, MD_RAD_ERRAMP_C, MD_RAD_ERRAMP_V, Md_rad_erramp)
}


def BMW_MOD_IfMgr_FocDt(BMWtqw_b_CdnActTqWhl_bo: InMeasurement, Status_antrieb_ist: InMeasurement, Status_mdred_egs_plaus: InMeasurement, BMWtqw_cw_FocDtExt_C: BigDecimal, BMWtqw_mask_RqrtGbxIntvFocDet_C: BigDecimal, BMWtqw_ti_RqrtGbxIntvFocDet_C: BigDecimal, BMWtqw_stb_FocDtHa_ub: OutMeasurement, BMWtqw_stb_FocDtHa_ub_BMWtqw_b_FocGbxHa_bc: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_IfMgr_FocDt(a2lBin: A2LBinAdapter): Unit = {

val BMWtqw_cw_FocDtExt_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_cw_FocDtExt_C")
val BMWtqw_mask_RqrtGbxIntvFocDet_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_mask_RqrtGbxIntvFocDet_C")
val BMWtqw_ti_RqrtGbxIntvFocDet_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_ti_RqrtGbxIntvFocDet_C")
val BMWtqw_b_CdnActTqWhl_bo: InMeasurement = a2lBin.measurement("BMWtqw_b_CdnActTqWhl_bo")
val Status_antrieb_ist: InMeasurement = a2lBin.measurement("Status_antrieb_ist")
val Status_mdred_egs_plaus: InMeasurement = a2lBin.measurement("Status_mdred_egs_plaus")
val BMWtqw_stb_FocDtHa_ub: OutMeasurement = a2lBin.measurement("BMWtqw_stb_FocDtHa_ub")
val BMWtqw_stb_FocDtHa_ub_BMWtqw_b_FocGbxHa_bc: OutMeasurement = a2lBin.measurement("BMWtqw_stb_FocDtHa_ub.BMWtqw_b_FocGbxHa_bc")

  BMW_MOD_IfMgr_FocDt(BMWtqw_b_CdnActTqWhl_bo, Status_antrieb_ist, Status_mdred_egs_plaus, BMWtqw_cw_FocDtExt_C, BMWtqw_mask_RqrtGbxIntvFocDet_C, BMWtqw_ti_RqrtGbxIntvFocDet_C, BMWtqw_stb_FocDtHa_ub, BMWtqw_stb_FocDtHa_ub_BMWtqw_b_FocGbxHa_bc)
}


def BMW_MOD_IfMgr_LimRecu(BMWmsa_stb_MsaDrr_ub: InMeasurement, BMWmsa_stb_MsaDrr_ub_B_fahrbereit: InMeasurement, BMWmsa_stb_MsaDrr_ub_B_fahrbereit_pwf: InMeasurement, Status_mdrekup: InMeasurement, K_INI_ST_SQ_REKUP: BigDecimal, K_MDREKUP_OFF: BigDecimal, K_MDREKUP_OK: BigDecimal, K_ST_SQ_REKUP_VAL: Array[BigDecimal], ST_SQ_REKUP_C: String, ST_SQ_REKUP_V: BigDecimal, St_sq_rekup: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_IfMgr_LimRecu(a2lBin: A2LBinAdapter): Unit = {

val K_INI_ST_SQ_REKUP: BigDecimal = a2lBin.readCharacteristicWithCast("K_INI_ST_SQ_REKUP")
val K_MDREKUP_OFF: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDREKUP_OFF")
val K_MDREKUP_OK: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDREKUP_OK")
val K_ST_SQ_REKUP_VAL: Array[BigDecimal] = a2lBin.readCharacteristicWithCast("K_ST_SQ_REKUP_VAL")
val ST_SQ_REKUP_C: String = a2lBin.readCharacteristicWithCast("ST_SQ_REKUP_C")
val ST_SQ_REKUP_V: BigDecimal = a2lBin.readCharacteristicWithCast("ST_SQ_REKUP_V")
val BMWmsa_stb_MsaDrr_ub: InMeasurement = a2lBin.measurement("BMWmsa_stb_MsaDrr_ub")
val BMWmsa_stb_MsaDrr_ub_B_fahrbereit: InMeasurement = a2lBin.measurement("BMWmsa_stb_MsaDrr_ub.B_fahrbereit")
val BMWmsa_stb_MsaDrr_ub_B_fahrbereit_pwf: InMeasurement = a2lBin.measurement("BMWmsa_stb_MsaDrr_ub.B_fahrbereit_pwf")
val Status_mdrekup: InMeasurement = a2lBin.measurement("Status_mdrekup")
val St_sq_rekup: OutMeasurement = a2lBin.measurement("St_sq_rekup")

  BMW_MOD_IfMgr_LimRecu(BMWmsa_stb_MsaDrr_ub, BMWmsa_stb_MsaDrr_ub_B_fahrbereit, BMWmsa_stb_MsaDrr_ub_B_fahrbereit_pwf, Status_mdrekup, K_INI_ST_SQ_REKUP, K_MDREKUP_OFF, K_MDREKUP_OK, K_ST_SQ_REKUP_VAL, ST_SQ_REKUP_C, ST_SQ_REKUP_V, St_sq_rekup)
}


def BMW_MOD_IfMgr_LimTqwExt(BMWtqc_tqw_DtHaMaxDyn_sw: InMeasurement, BMWtqc_tqw_DtHaMaxStat_sw: InMeasurement, BMWtqc_tqw_DtHaMinDyn_sw: InMeasurement, BMWtqc_tqw_DtHaMinStat_sw: InMeasurement, BMWtqc_tqw_EngDfcoMinStat_sw: InMeasurement, BMWtqc_tqw_EngInjMinStat_sw: InMeasurement, BMWtqc_tqw_EngMinDyn_sw: InMeasurement, BMWtqw_fac_StatLossHa_ub: InMeasurement, BMWtqw_stb_FocDtHa_ub: InMeasurement, BMWtqw_stb_FocDtHa_ub_BMWtqw_b_FocGbxHa_bc: InMeasurement, BMWtqw_tqw_DtHaMinStatIsc_sw: InMeasurement, BMWtqw_tqw_DynLossHa_sw: InMeasurement, BMWtqw_tqw_StatLossHa_sw: InMeasurement, Brtorqsum_plaus: InMeasurement, Gang_gb1_ist: InMeasurement, Md_rad_dynverl: InMeasurement, Md_rad_kriech: InMeasurement, Md_rad_schlepp_soll: InMeasurement, St_fahrzust_fzg: InMeasurement, Stat_vmbm: InMeasurement, Status_mdrekup: InMeasurement, V_fzg_fahrtricht: InMeasurement, BMWtqw_cw_CmpLossLimExt_C: BigDecimal, BMWtqw_cw_CmpLossRecuExt_C: BigDecimal, BMWtqw_fac_LimRecuBndDyno_C: BigDecimal, BMWtqw_mask_LimRecuBndDyno_C: BigDecimal, BMWtqw_tqw_LimRecuBnd_M: MapType[BigDecimal, BigDecimal, BigDecimal], BMWtqw_tqw_WhlEmRecuHaLimd_T: CurveType[BigDecimal, BigDecimal], CW_NEDI_LLR_CAN: Array[String], KL_MDREKUP_GRDUP: CurveType[BigDecimal, BigDecimal], K_MDREKU_GRDDN: BigDecimal, K_MDREKU_START: BigDecimal, K_MD_OFFS_EA_MIN_RM_2_EF: BigDecimal, MD_RAD_ISTLMMIN_C: String, MD_RAD_ISTLMMIN_V: BigDecimal, MD_RAD_ISTLM_C: String, MD_RAD_ISTLM_V: BigDecimal, MD_RAD_MAX_C: String, MD_RAD_MAX_HA_C: String, MD_RAD_MAX_HA_V: BigDecimal, MD_RAD_MAX_V: BigDecimal, MD_RAD_MIN_C: String, MD_RAD_MIN_HA_C: String, MD_RAD_MIN_HA_V: BigDecimal, MD_RAD_MIN_V: BigDecimal, MD_RAD_REKUPMX_C: String, MD_RAD_REKUPMX_V: BigDecimal, MD_RAD_SCHLEPP_C: String, MD_RAD_SCHLEPP_V: BigDecimal, S_CP422535: String, S_EAPOT_4_EF_EA_MIN_RM: String, S_NOCP354196MASSM: String, S_REKUKOMP: String, S_USE_REKUPMX_ROH_4_TOPBOT: String, BMWtqw_swi_CfgDt2VehFa_C: BigDecimal, BMWtqw_swi_CfgDt2VehRa_C: BigDecimal, Md_rad_brems_antr_max_pred: OutMeasurement, Md_rad_istlm: OutMeasurement, Md_rad_istlmmin: OutMeasurement, Md_rad_max: OutMeasurement, Md_rad_max_ha: OutMeasurement, Md_rad_min: OutMeasurement, Md_rad_min_ha: OutMeasurement, Md_rad_rekupmx: OutMeasurement, Md_rad_schlepp: OutMeasurement, Md_rad_schlepp_od: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_IfMgr_LimTqwExt(a2lBin: A2LBinAdapter): Unit = {

val BMWtqw_cw_CmpLossLimExt_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_cw_CmpLossLimExt_C")
val BMWtqw_cw_CmpLossRecuExt_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_cw_CmpLossRecuExt_C")
val BMWtqw_fac_LimRecuBndDyno_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_fac_LimRecuBndDyno_C")
val BMWtqw_mask_LimRecuBndDyno_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_mask_LimRecuBndDyno_C")
val BMWtqw_tqw_LimRecuBnd_M: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtqw_tqw_LimRecuBnd_M")
val BMWtqw_tqw_WhlEmRecuHaLimd_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtqw_tqw_WhlEmRecuHaLimd_T")
val CW_NEDI_LLR_CAN: Array[String] = a2lBin.readCharacteristicWithCast("CW_NEDI_LLR_CAN")
val KL_MDREKUP_GRDUP: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MDREKUP_GRDUP")
val K_MDREKU_GRDDN: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDREKU_GRDDN")
val K_MDREKU_START: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDREKU_START")
val K_MD_OFFS_EA_MIN_RM_2_EF: BigDecimal = a2lBin.readCharacteristicWithCast("K_MD_OFFS_EA_MIN_RM_2_EF")
val MD_RAD_ISTLMMIN_C: String = a2lBin.readCharacteristicWithCast("MD_RAD_ISTLMMIN_C")
val MD_RAD_ISTLMMIN_V: BigDecimal = a2lBin.readCharacteristicWithCast("MD_RAD_ISTLMMIN_V")
val MD_RAD_ISTLM_C: String = a2lBin.readCharacteristicWithCast("MD_RAD_ISTLM_C")
val MD_RAD_ISTLM_V: BigDecimal = a2lBin.readCharacteristicWithCast("MD_RAD_ISTLM_V")
val MD_RAD_MAX_C: String = a2lBin.readCharacteristicWithCast("MD_RAD_MAX_C")
val MD_RAD_MAX_HA_C: String = a2lBin.readCharacteristicWithCast("MD_RAD_MAX_HA_C")
val MD_RAD_MAX_HA_V: BigDecimal = a2lBin.readCharacteristicWithCast("MD_RAD_MAX_HA_V")
val MD_RAD_MAX_V: BigDecimal = a2lBin.readCharacteristicWithCast("MD_RAD_MAX_V")
val MD_RAD_MIN_C: String = a2lBin.readCharacteristicWithCast("MD_RAD_MIN_C")
val MD_RAD_MIN_HA_C: String = a2lBin.readCharacteristicWithCast("MD_RAD_MIN_HA_C")
val MD_RAD_MIN_HA_V: BigDecimal = a2lBin.readCharacteristicWithCast("MD_RAD_MIN_HA_V")
val MD_RAD_MIN_V: BigDecimal = a2lBin.readCharacteristicWithCast("MD_RAD_MIN_V")
val MD_RAD_REKUPMX_C: String = a2lBin.readCharacteristicWithCast("MD_RAD_REKUPMX_C")
val MD_RAD_REKUPMX_V: BigDecimal = a2lBin.readCharacteristicWithCast("MD_RAD_REKUPMX_V")
val MD_RAD_SCHLEPP_C: String = a2lBin.readCharacteristicWithCast("MD_RAD_SCHLEPP_C")
val MD_RAD_SCHLEPP_V: BigDecimal = a2lBin.readCharacteristicWithCast("MD_RAD_SCHLEPP_V")
val S_CP422535: String = a2lBin.readCharacteristicWithCast("S_CP422535")
val S_EAPOT_4_EF_EA_MIN_RM: String = a2lBin.readCharacteristicWithCast("S_EAPOT_4_EF_EA_MIN_RM")
val S_NOCP354196MASSM: String = a2lBin.readCharacteristicWithCast("S_NOCP354196MASSM")
val S_REKUKOMP: String = a2lBin.readCharacteristicWithCast("S_REKUKOMP")
val S_USE_REKUPMX_ROH_4_TOPBOT: String = a2lBin.readCharacteristicWithCast("S_USE_REKUPMX_ROH_4_TOPBOT")
val BMWtqw_swi_CfgDt2VehFa_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_swi_CfgDt2VehFa_C")
val BMWtqw_swi_CfgDt2VehRa_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_swi_CfgDt2VehRa_C")
val BMWtqc_tqw_DtHaMaxDyn_sw: InMeasurement = a2lBin.measurement("BMWtqc_tqw_DtHaMaxDyn_sw")
val BMWtqc_tqw_DtHaMaxStat_sw: InMeasurement = a2lBin.measurement("BMWtqc_tqw_DtHaMaxStat_sw")
val BMWtqc_tqw_DtHaMinDyn_sw: InMeasurement = a2lBin.measurement("BMWtqc_tqw_DtHaMinDyn_sw")
val BMWtqc_tqw_DtHaMinStat_sw: InMeasurement = a2lBin.measurement("BMWtqc_tqw_DtHaMinStat_sw")
val BMWtqc_tqw_EngDfcoMinStat_sw: InMeasurement = a2lBin.measurement("BMWtqc_tqw_EngDfcoMinStat_sw")
val BMWtqc_tqw_EngInjMinStat_sw: InMeasurement = a2lBin.measurement("BMWtqc_tqw_EngInjMinStat_sw")
val BMWtqc_tqw_EngMinDyn_sw: InMeasurement = a2lBin.measurement("BMWtqc_tqw_EngMinDyn_sw")
val BMWtqw_fac_StatLossHa_ub: InMeasurement = a2lBin.measurement("BMWtqw_fac_StatLossHa_ub")
val BMWtqw_stb_FocDtHa_ub: InMeasurement = a2lBin.measurement("BMWtqw_stb_FocDtHa_ub")
val BMWtqw_stb_FocDtHa_ub_BMWtqw_b_FocGbxHa_bc: InMeasurement = a2lBin.measurement("BMWtqw_stb_FocDtHa_ub.BMWtqw_b_FocGbxHa_bc")
val BMWtqw_tqw_DtHaMinStatIsc_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_DtHaMinStatIsc_sw")
val BMWtqw_tqw_DynLossHa_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_DynLossHa_sw")
val BMWtqw_tqw_StatLossHa_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_StatLossHa_sw")
val Brtorqsum_plaus: InMeasurement = a2lBin.measurement("Brtorqsum_plaus")
val Gang_gb1_ist: InMeasurement = a2lBin.measurement("Gang_gb1_ist")
val Md_rad_dynverl: InMeasurement = a2lBin.measurement("Md_rad_dynverl")
val Md_rad_kriech: InMeasurement = a2lBin.measurement("Md_rad_kriech")
val Md_rad_schlepp_soll: InMeasurement = a2lBin.measurement("Md_rad_schlepp_soll")
val St_fahrzust_fzg: InMeasurement = a2lBin.measurement("St_fahrzust_fzg")
val Stat_vmbm: InMeasurement = a2lBin.measurement("Stat_vmbm")
val Status_mdrekup: InMeasurement = a2lBin.measurement("Status_mdrekup")
val V_fzg_fahrtricht: InMeasurement = a2lBin.measurement("V_fzg_fahrtricht")
val Md_rad_brems_antr_max_pred: OutMeasurement = a2lBin.measurement("Md_rad_brems_antr_max_pred")
val Md_rad_istlm: OutMeasurement = a2lBin.measurement("Md_rad_istlm")
val Md_rad_istlmmin: OutMeasurement = a2lBin.measurement("Md_rad_istlmmin")
val Md_rad_max: OutMeasurement = a2lBin.measurement("Md_rad_max")
val Md_rad_max_ha: OutMeasurement = a2lBin.measurement("Md_rad_max_ha")
val Md_rad_min: OutMeasurement = a2lBin.measurement("Md_rad_min")
val Md_rad_min_ha: OutMeasurement = a2lBin.measurement("Md_rad_min_ha")
val Md_rad_rekupmx: OutMeasurement = a2lBin.measurement("Md_rad_rekupmx")
val Md_rad_schlepp: OutMeasurement = a2lBin.measurement("Md_rad_schlepp")
val Md_rad_schlepp_od: OutMeasurement = a2lBin.measurement("Md_rad_schlepp_od")

  BMW_MOD_IfMgr_LimTqwExt(BMWtqc_tqw_DtHaMaxDyn_sw, BMWtqc_tqw_DtHaMaxStat_sw, BMWtqc_tqw_DtHaMinDyn_sw, BMWtqc_tqw_DtHaMinStat_sw, BMWtqc_tqw_EngDfcoMinStat_sw, BMWtqc_tqw_EngInjMinStat_sw, BMWtqc_tqw_EngMinDyn_sw, BMWtqw_fac_StatLossHa_ub, BMWtqw_stb_FocDtHa_ub, BMWtqw_stb_FocDtHa_ub_BMWtqw_b_FocGbxHa_bc, BMWtqw_tqw_DtHaMinStatIsc_sw, BMWtqw_tqw_DynLossHa_sw, BMWtqw_tqw_StatLossHa_sw, Brtorqsum_plaus, Gang_gb1_ist, Md_rad_dynverl, Md_rad_kriech, Md_rad_schlepp_soll, St_fahrzust_fzg, Stat_vmbm, Status_mdrekup, V_fzg_fahrtricht, BMWtqw_cw_CmpLossLimExt_C, BMWtqw_cw_CmpLossRecuExt_C, BMWtqw_fac_LimRecuBndDyno_C, BMWtqw_mask_LimRecuBndDyno_C, BMWtqw_tqw_LimRecuBnd_M, BMWtqw_tqw_WhlEmRecuHaLimd_T, CW_NEDI_LLR_CAN, KL_MDREKUP_GRDUP, K_MDREKU_GRDDN, K_MDREKU_START, K_MD_OFFS_EA_MIN_RM_2_EF, MD_RAD_ISTLMMIN_C, MD_RAD_ISTLMMIN_V, MD_RAD_ISTLM_C, MD_RAD_ISTLM_V, MD_RAD_MAX_C, MD_RAD_MAX_HA_C, MD_RAD_MAX_HA_V, MD_RAD_MAX_V, MD_RAD_MIN_C, MD_RAD_MIN_HA_C, MD_RAD_MIN_HA_V, MD_RAD_MIN_V, MD_RAD_REKUPMX_C, MD_RAD_REKUPMX_V, MD_RAD_SCHLEPP_C, MD_RAD_SCHLEPP_V, S_CP422535, S_EAPOT_4_EF_EA_MIN_RM, S_NOCP354196MASSM, S_REKUKOMP, S_USE_REKUPMX_ROH_4_TOPBOT, BMWtqw_swi_CfgDt2VehFa_C, BMWtqw_swi_CfgDt2VehRa_C, Md_rad_brems_antr_max_pred, Md_rad_istlm, Md_rad_istlmmin, Md_rad_max, Md_rad_max_ha, Md_rad_min, Md_rad_min_ha, Md_rad_rekupmx, Md_rad_schlepp, Md_rad_schlepp_od)
}


def BMW_MOD_IfMgr_LossDt(BMWtqc_Fac_StatLossHaxl: InMeasurement, BMWtqc_Tqw_DynLossHaxl: InMeasurement, BMWtqc_Tqw_StatLossHaxl: InMeasurement, BMWtqw_fac_StatLossHa_ub: OutMeasurement, BMWtqw_tqw_DynLossHa_sw: OutMeasurement, BMWtqw_tqw_StatLossHa_sw: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_IfMgr_LossDt(a2lBin: A2LBinAdapter): Unit = {


val BMWtqc_Fac_StatLossHaxl: InMeasurement = a2lBin.measurement("BMWtqc_Fac_StatLossHaxl")
val BMWtqc_Tqw_DynLossHaxl: InMeasurement = a2lBin.measurement("BMWtqc_Tqw_DynLossHaxl")
val BMWtqc_Tqw_StatLossHaxl: InMeasurement = a2lBin.measurement("BMWtqc_Tqw_StatLossHaxl")
val BMWtqw_fac_StatLossHa_ub: OutMeasurement = a2lBin.measurement("BMWtqw_fac_StatLossHa_ub")
val BMWtqw_tqw_DynLossHa_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_DynLossHa_sw")
val BMWtqw_tqw_StatLossHa_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_StatLossHa_sw")

  BMW_MOD_IfMgr_LossDt(BMWtqc_Fac_StatLossHaxl, BMWtqc_Tqw_DynLossHaxl, BMWtqc_Tqw_StatLossHaxl, BMWtqw_fac_StatLossHa_ub, BMWtqw_tqw_DynLossHa_sw, BMWtqw_tqw_StatLossHa_sw)
}


def BMW_MOD_IfMgr_OpmRecu(Ba_em1_ist_plaus: InMeasurement, Ba_em2_ist_plaus: InMeasurement, Fahrstufe_ist: InMeasurement, Pwg_ist: InMeasurement, St_getrdaten: InMeasurement, St_getrdaten_B_gangwechsel_gs: InMeasurement, BMWtqw_cw_OpmEm1RecuDiRev_C: BigDecimal, BMWtqw_cw_OpmEm1RecuDi_C: BigDecimal, BMWtqw_cw_OpmEm2RecuDiRev_C: BigDecimal, BMWtqw_cw_OpmEm2RecuDi_C: BigDecimal, CW_CONF_MDREKUP: BigDecimal, KL_MDREKUP_KLASS: CurveType[BigDecimal, BigDecimal], K_MDREKUP_KLASS_01: BigDecimal, K_MDREKUP_KLASS_02: BigDecimal, K_MDREKUP_KLASS_03: BigDecimal, K_MDREKUP_KLASS_05: BigDecimal, K_MDREKUP_KLASS_06: BigDecimal, K_MDREKUP_KLASS_07: BigDecimal, K_MDREKUP_KLASS_08: BigDecimal, K_PWGMIN_OFFSET: BigDecimal, K_PWGMIN_SCHWELLE: BigDecimal, K_T_MDREKUP_GS: BigDecimal, Status_mdrekup: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_IfMgr_OpmRecu(a2lBin: A2LBinAdapter): Unit = {

val BMWtqw_cw_OpmEm1RecuDiRev_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_cw_OpmEm1RecuDiRev_C")
val BMWtqw_cw_OpmEm1RecuDi_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_cw_OpmEm1RecuDi_C")
val BMWtqw_cw_OpmEm2RecuDiRev_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_cw_OpmEm2RecuDiRev_C")
val BMWtqw_cw_OpmEm2RecuDi_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_cw_OpmEm2RecuDi_C")
val CW_CONF_MDREKUP: BigDecimal = a2lBin.readCharacteristicWithCast("CW_CONF_MDREKUP")
val KL_MDREKUP_KLASS: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MDREKUP_KLASS")
val K_MDREKUP_KLASS_01: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDREKUP_KLASS_01")
val K_MDREKUP_KLASS_02: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDREKUP_KLASS_02")
val K_MDREKUP_KLASS_03: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDREKUP_KLASS_03")
val K_MDREKUP_KLASS_05: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDREKUP_KLASS_05")
val K_MDREKUP_KLASS_06: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDREKUP_KLASS_06")
val K_MDREKUP_KLASS_07: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDREKUP_KLASS_07")
val K_MDREKUP_KLASS_08: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDREKUP_KLASS_08")
val K_PWGMIN_OFFSET: BigDecimal = a2lBin.readCharacteristicWithCast("K_PWGMIN_OFFSET")
val K_PWGMIN_SCHWELLE: BigDecimal = a2lBin.readCharacteristicWithCast("K_PWGMIN_SCHWELLE")
val K_T_MDREKUP_GS: BigDecimal = a2lBin.readCharacteristicWithCast("K_T_MDREKUP_GS")
val Ba_em1_ist_plaus: InMeasurement = a2lBin.measurement("Ba_em1_ist_plaus")
val Ba_em2_ist_plaus: InMeasurement = a2lBin.measurement("Ba_em2_ist_plaus")
val Fahrstufe_ist: InMeasurement = a2lBin.measurement("Fahrstufe_ist")
val Pwg_ist: InMeasurement = a2lBin.measurement("Pwg_ist")
val St_getrdaten: InMeasurement = a2lBin.measurement("St_getrdaten")
val St_getrdaten_B_gangwechsel_gs: InMeasurement = a2lBin.measurement("St_getrdaten.B_gangwechsel_gs")
val Status_mdrekup: OutMeasurement = a2lBin.measurement("Status_mdrekup")

  BMW_MOD_IfMgr_OpmRecu(Ba_em1_ist_plaus, Ba_em2_ist_plaus, Fahrstufe_ist, Pwg_ist, St_getrdaten, St_getrdaten_B_gangwechsel_gs, BMWtqw_cw_OpmEm1RecuDiRev_C, BMWtqw_cw_OpmEm1RecuDi_C, BMWtqw_cw_OpmEm2RecuDiRev_C, BMWtqw_cw_OpmEm2RecuDi_C, CW_CONF_MDREKUP, KL_MDREKUP_KLASS, K_MDREKUP_KLASS_01, K_MDREKUP_KLASS_02, K_MDREKUP_KLASS_03, K_MDREKUP_KLASS_05, K_MDREKUP_KLASS_06, K_MDREKUP_KLASS_07, K_MDREKUP_KLASS_08, K_PWGMIN_OFFSET, K_PWGMIN_SCHWELLE, K_T_MDREKUP_GS, Status_mdrekup)
}


def BMW_MOD_IfMgr_SpTqwExt(BMWtqw_fac_StatLossHa_ub: InMeasurement, BMWtqw_tqw_AxcHaLdcSpFildEfR_sw: InMeasurement, BMWtqw_tqw_AxcHaLdcSpFild_sw: InMeasurement, BMWtqw_tqw_AxcHaSpFild_sw: InMeasurement, BMWtqw_tqw_AxcHaSpUnf_sw: InMeasurement, BMWtqw_tqw_DtHaAvlStatIsc_sw: InMeasurement, BMWtqw_tqw_DtHaMinStatIsc_sw: InMeasurement, BMWtqw_tqw_StatLossHa_sw: InMeasurement, Md_rad_fzdyn_getr: InMeasurement, Md_rad_soll: InMeasurement, Md_rad_wunsch: InMeasurement, Md_rad_wunsch_fas: InMeasurement, Md_rad_wunsch_vb: InMeasurement, St_fas_mradsoll: InMeasurement, St_fw: InMeasurement, St_fw_B_hschalt_komb: InMeasurement, St_fw_B_ldm_akt04: InMeasurement, St_fw_B_ldm_off: InMeasurement, St_fw_B_ldm_offhs: InMeasurement, St_fw_B_ldm_offreg: InMeasurement, St_fw_B_ldm_offrs: InMeasurement, St_fw_B_rschalt_komb: InMeasurement, St_mdinfo_maw: InMeasurement, St_mdinfo_maw_B_fd_max_akt: InMeasurement, St_mdinfo_maw_B_fd_min_akt: InMeasurement, BMWtqw_swi_AxcHaExtSp_C: BigDecimal, CW_NEDI_LLRADD_CAN: Array[String], K_MDSSM_BEGR_MD_RAD: BigDecimal, MD_RAD_MXK_GES_C: String, MD_RAD_MXK_GES_V: BigDecimal, MD_RAD_MXK_HA_C: String, MD_RAD_MXK_HA_V: BigDecimal, MD_RAD_RSOLL_C: String, MD_RAD_RSOLL_V: BigDecimal, MD_RAD_WUNSCH_GE_C: String, MD_RAD_WUNSCH_GE_V: BigDecimal, MD_RAD_WUNSCH_LIM_C: String, MD_RAD_WUNSCH_LIM_V: BigDecimal, BMWtqw_swi_CfgDt2VehFa_C: BigDecimal, BMWtqw_swi_CfgDt2VehRa_C: BigDecimal, CW_NEDI_LLR_CAN: Array[String], Md_rad_mxk_ges: OutMeasurement, Md_rad_mxk_ha: OutMeasurement, Md_rad_rsoll: OutMeasurement, Md_rad_wunsch_ge: OutMeasurement, Md_rad_wunsch_lim: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_IfMgr_SpTqwExt(a2lBin: A2LBinAdapter): Unit = {

val BMWtqw_swi_AxcHaExtSp_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_swi_AxcHaExtSp_C")
val CW_NEDI_LLRADD_CAN: Array[String] = a2lBin.readCharacteristicWithCast("CW_NEDI_LLRADD_CAN")
val K_MDSSM_BEGR_MD_RAD: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDSSM_BEGR_MD_RAD")
val MD_RAD_MXK_GES_C: String = a2lBin.readCharacteristicWithCast("MD_RAD_MXK_GES_C")
val MD_RAD_MXK_GES_V: BigDecimal = a2lBin.readCharacteristicWithCast("MD_RAD_MXK_GES_V")
val MD_RAD_MXK_HA_C: String = a2lBin.readCharacteristicWithCast("MD_RAD_MXK_HA_C")
val MD_RAD_MXK_HA_V: BigDecimal = a2lBin.readCharacteristicWithCast("MD_RAD_MXK_HA_V")
val MD_RAD_RSOLL_C: String = a2lBin.readCharacteristicWithCast("MD_RAD_RSOLL_C")
val MD_RAD_RSOLL_V: BigDecimal = a2lBin.readCharacteristicWithCast("MD_RAD_RSOLL_V")
val MD_RAD_WUNSCH_GE_C: String = a2lBin.readCharacteristicWithCast("MD_RAD_WUNSCH_GE_C")
val MD_RAD_WUNSCH_GE_V: BigDecimal = a2lBin.readCharacteristicWithCast("MD_RAD_WUNSCH_GE_V")
val MD_RAD_WUNSCH_LIM_C: String = a2lBin.readCharacteristicWithCast("MD_RAD_WUNSCH_LIM_C")
val MD_RAD_WUNSCH_LIM_V: BigDecimal = a2lBin.readCharacteristicWithCast("MD_RAD_WUNSCH_LIM_V")
val BMWtqw_swi_CfgDt2VehFa_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_swi_CfgDt2VehFa_C")
val BMWtqw_swi_CfgDt2VehRa_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_swi_CfgDt2VehRa_C")
val CW_NEDI_LLR_CAN: Array[String] = a2lBin.readCharacteristicWithCast("CW_NEDI_LLR_CAN")
val BMWtqw_fac_StatLossHa_ub: InMeasurement = a2lBin.measurement("BMWtqw_fac_StatLossHa_ub")
val BMWtqw_tqw_AxcHaLdcSpFildEfR_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcHaLdcSpFildEfR_sw")
val BMWtqw_tqw_AxcHaLdcSpFild_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcHaLdcSpFild_sw")
val BMWtqw_tqw_AxcHaSpFild_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcHaSpFild_sw")
val BMWtqw_tqw_AxcHaSpUnf_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcHaSpUnf_sw")
val BMWtqw_tqw_DtHaAvlStatIsc_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_DtHaAvlStatIsc_sw")
val BMWtqw_tqw_DtHaMinStatIsc_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_DtHaMinStatIsc_sw")
val BMWtqw_tqw_StatLossHa_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_StatLossHa_sw")
val Md_rad_fzdyn_getr: InMeasurement = a2lBin.measurement("Md_rad_fzdyn_getr")
val Md_rad_soll: InMeasurement = a2lBin.measurement("Md_rad_soll")
val Md_rad_wunsch: InMeasurement = a2lBin.measurement("Md_rad_wunsch")
val Md_rad_wunsch_fas: InMeasurement = a2lBin.measurement("Md_rad_wunsch_fas")
val Md_rad_wunsch_vb: InMeasurement = a2lBin.measurement("Md_rad_wunsch_vb")
val St_fas_mradsoll: InMeasurement = a2lBin.measurement("St_fas_mradsoll")
val St_fw: InMeasurement = a2lBin.measurement("St_fw")
val St_fw_B_hschalt_komb: InMeasurement = a2lBin.measurement("St_fw.B_hschalt_komb")
val St_fw_B_ldm_akt04: InMeasurement = a2lBin.measurement("St_fw.B_ldm_akt04")
val St_fw_B_ldm_off: InMeasurement = a2lBin.measurement("St_fw.B_ldm_off")
val St_fw_B_ldm_offhs: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offhs")
val St_fw_B_ldm_offreg: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offreg")
val St_fw_B_ldm_offrs: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offrs")
val St_fw_B_rschalt_komb: InMeasurement = a2lBin.measurement("St_fw.B_rschalt_komb")
val St_mdinfo_maw: InMeasurement = a2lBin.measurement("St_mdinfo_maw")
val St_mdinfo_maw_B_fd_max_akt: InMeasurement = a2lBin.measurement("St_mdinfo_maw.B_fd_max_akt")
val St_mdinfo_maw_B_fd_min_akt: InMeasurement = a2lBin.measurement("St_mdinfo_maw.B_fd_min_akt")
val Md_rad_mxk_ges: OutMeasurement = a2lBin.measurement("Md_rad_mxk_ges")
val Md_rad_mxk_ha: OutMeasurement = a2lBin.measurement("Md_rad_mxk_ha")
val Md_rad_rsoll: OutMeasurement = a2lBin.measurement("Md_rad_rsoll")
val Md_rad_wunsch_ge: OutMeasurement = a2lBin.measurement("Md_rad_wunsch_ge")
val Md_rad_wunsch_lim: OutMeasurement = a2lBin.measurement("Md_rad_wunsch_lim")

  BMW_MOD_IfMgr_SpTqwExt(BMWtqw_fac_StatLossHa_ub, BMWtqw_tqw_AxcHaLdcSpFildEfR_sw, BMWtqw_tqw_AxcHaLdcSpFild_sw, BMWtqw_tqw_AxcHaSpFild_sw, BMWtqw_tqw_AxcHaSpUnf_sw, BMWtqw_tqw_DtHaAvlStatIsc_sw, BMWtqw_tqw_DtHaMinStatIsc_sw, BMWtqw_tqw_StatLossHa_sw, Md_rad_fzdyn_getr, Md_rad_soll, Md_rad_wunsch, Md_rad_wunsch_fas, Md_rad_wunsch_vb, St_fas_mradsoll, St_fw, St_fw_B_hschalt_komb, St_fw_B_ldm_akt04, St_fw_B_ldm_off, St_fw_B_ldm_offhs, St_fw_B_ldm_offreg, St_fw_B_ldm_offrs, St_fw_B_rschalt_komb, St_mdinfo_maw, St_mdinfo_maw_B_fd_max_akt, St_mdinfo_maw_B_fd_min_akt, BMWtqw_swi_AxcHaExtSp_C, CW_NEDI_LLRADD_CAN, K_MDSSM_BEGR_MD_RAD, MD_RAD_MXK_GES_C, MD_RAD_MXK_GES_V, MD_RAD_MXK_HA_C, MD_RAD_MXK_HA_V, MD_RAD_RSOLL_C, MD_RAD_RSOLL_V, MD_RAD_WUNSCH_GE_C, MD_RAD_WUNSCH_GE_V, MD_RAD_WUNSCH_LIM_C, MD_RAD_WUNSCH_LIM_V, BMWtqw_swi_CfgDt2VehFa_C, BMWtqw_swi_CfgDt2VehRa_C, CW_NEDI_LLR_CAN, Md_rad_mxk_ges, Md_rad_mxk_ha, Md_rad_rsoll, Md_rad_wunsch_ge, Md_rad_wunsch_lim)
}


def BMW_MOD_IfMgr_TqwDynLoss(BMWtqw_fac_StatLossHa_ub: InMeasurement, BMWtqw_stb_FocDtHa_ub: InMeasurement, BMWtqw_stb_FocDtHa_ub_BMWtqw_b_FocGbxHa_bc: InMeasurement, BMWtqw_tqw_DynLossHa_sw: InMeasurement, BMWtqw_tqw_StatLossHa_sw: InMeasurement, BMWtqw_cw_DynLossExt_C: BigDecimal, MD_RAD_DYNVERL_C: String, MD_RAD_DYNVERL_HA_C: String, MD_RAD_DYNVERL_HA_V: BigDecimal, MD_RAD_DYNVERL_V: BigDecimal, BMWtqw_swi_CfgDt2VehFa_C: BigDecimal, BMWtqw_swi_CfgDt2VehRa_C: BigDecimal, Md_rad_dynverl: OutMeasurement, Md_rad_dynverl_ha: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_IfMgr_TqwDynLoss(a2lBin: A2LBinAdapter): Unit = {

val BMWtqw_cw_DynLossExt_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_cw_DynLossExt_C")
val MD_RAD_DYNVERL_C: String = a2lBin.readCharacteristicWithCast("MD_RAD_DYNVERL_C")
val MD_RAD_DYNVERL_HA_C: String = a2lBin.readCharacteristicWithCast("MD_RAD_DYNVERL_HA_C")
val MD_RAD_DYNVERL_HA_V: BigDecimal = a2lBin.readCharacteristicWithCast("MD_RAD_DYNVERL_HA_V")
val MD_RAD_DYNVERL_V: BigDecimal = a2lBin.readCharacteristicWithCast("MD_RAD_DYNVERL_V")
val BMWtqw_swi_CfgDt2VehFa_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_swi_CfgDt2VehFa_C")
val BMWtqw_swi_CfgDt2VehRa_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_swi_CfgDt2VehRa_C")
val BMWtqw_fac_StatLossHa_ub: InMeasurement = a2lBin.measurement("BMWtqw_fac_StatLossHa_ub")
val BMWtqw_stb_FocDtHa_ub: InMeasurement = a2lBin.measurement("BMWtqw_stb_FocDtHa_ub")
val BMWtqw_stb_FocDtHa_ub_BMWtqw_b_FocGbxHa_bc: InMeasurement = a2lBin.measurement("BMWtqw_stb_FocDtHa_ub.BMWtqw_b_FocGbxHa_bc")
val BMWtqw_tqw_DynLossHa_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_DynLossHa_sw")
val BMWtqw_tqw_StatLossHa_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_StatLossHa_sw")
val Md_rad_dynverl: OutMeasurement = a2lBin.measurement("Md_rad_dynverl")
val Md_rad_dynverl_ha: OutMeasurement = a2lBin.measurement("Md_rad_dynverl_ha")

  BMW_MOD_IfMgr_TqwDynLoss(BMWtqw_fac_StatLossHa_ub, BMWtqw_stb_FocDtHa_ub, BMWtqw_stb_FocDtHa_ub_BMWtqw_b_FocGbxHa_bc, BMWtqw_tqw_DynLossHa_sw, BMWtqw_tqw_StatLossHa_sw, BMWtqw_cw_DynLossExt_C, MD_RAD_DYNVERL_C, MD_RAD_DYNVERL_HA_C, MD_RAD_DYNVERL_HA_V, MD_RAD_DYNVERL_V, BMWtqw_swi_CfgDt2VehFa_C, BMWtqw_swi_CfgDt2VehRa_C, Md_rad_dynverl, Md_rad_dynverl_ha)
}


def BMW_MOD_MaBgr(BMWtqc_Fac_StatLossHaxl: InMeasurement, BMWtqc_Rat_GbxWhlHaxl: InMeasurement, BMWtqc_Spd_ActLeWhlHaxl: InMeasurement, BMWtqc_Spd_ActRiWhlHaxl: InMeasurement, BMWtqc_Tqw_StatLossHaxl: InMeasurement, B_leistungsmessung: InMeasurement, Dmdgsoll_hybachse: InMeasurement, I_ha: InMeasurement, Md_lmv_v_ist: InMeasurement, Md_lmv_v_soll_plaus: InMeasurement, Nkw: InMeasurement, On_antriebsart_cod: InMeasurement, St_MDGWF_01: InMeasurement, St_MDGWF_01_B_dash_mdgwf: InMeasurement, St_MDGWF_01_B_dmls: InMeasurement, St_MDGWF_01_B_dp: InMeasurement, St_MDGWF_01_B_ls: InMeasurement, St_MDGWF_01_B_lsd_mdgwf: InMeasurement, St_MDGWF_01_B_vnull: InMeasurement, St_mdanfahr_cc: InMeasurement, St_mdanfahr_cc_B_anfahr_nmax_disp_flag: InMeasurement, St_mdanfahr_cc_B_rennstart_aktiv: InMeasurement, St_mdar0: InMeasurement, St_mdar0_B_edp: InMeasurement, St_mdar0_B_elsd: InMeasurement, St_mdar0_B_lsd: InMeasurement, St_oz: InMeasurement, St_oz_B_anhang: InMeasurement, St_wk_plaus: InMeasurement, Var_hs: InMeasurement, W_afs: InMeasurement, CW_MDRADMAX_NMAX: BigDecimal, CW_MDRADMAX_WEL: BigDecimal, KL_MDABMAX_LMV: CurveType[BigDecimal, BigDecimal], KL_MDRADMAX_ANH: CurveType[BigDecimal, BigDecimal], KL_MDRADMAX_BEGR: CurveType[BigDecimal, BigDecimal], KL_MDRADMAX_LMV: CurveType[BigDecimal, BigDecimal], KL_MDRADMAX_NOT: CurveType[BigDecimal, BigDecimal], KL_PMAX_TGHAS: CurveType[BigDecimal, BigDecimal], K_MDGW_MAX: BigDecimal, K_MDGW_MAX_HS: BigDecimal, K_MDGW_MAX_LM: BigDecimal, K_MDGW_MAX_LM_HS: BigDecimal, K_MDKRADBEGR_NMAX: BigDecimal, K_MDKRADBEGR_NMAX_GRENZ: BigDecimal, K_MDRADMAX_LMV: BigDecimal, K_MDRAD_AW_MAX: BigDecimal, K_MDRAD_AW_MAX_HS: BigDecimal, K_MDRAD_AW_MAX_LM: BigDecimal, K_MDRAD_AW_MAX_LM_HS: BigDecimal, K_MDRAD_BEGR_GRD_DN: BigDecimal, K_MDRAD_BEGR_GRD_UP_ERSATZ: BigDecimal, K_MDRAD_MAX_ALLRAD: BigDecimal, K_MDRAD_MAX_ALLRAD_HS: BigDecimal, K_MDRAD_MAX_BEGR_GRD_UP: BigDecimal, K_TD_MDBGR_FAHRLEISTUNG: BigDecimal, K_TGHAS_MAX: BigDecimal, S_MDRAD_GRD_UP_ERSATZ: String, BMWtqw_tqwi_MaxLimHaxl: OutMeasurement, BMWtqw_tqwi_MaxLimTReDftl: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_MaBgr(a2lBin: A2LBinAdapter): Unit = {

val CW_MDRADMAX_NMAX: BigDecimal = a2lBin.readCharacteristicWithCast("CW_MDRADMAX_NMAX")
val CW_MDRADMAX_WEL: BigDecimal = a2lBin.readCharacteristicWithCast("CW_MDRADMAX_WEL")
val KL_MDABMAX_LMV: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MDABMAX_LMV")
val KL_MDRADMAX_ANH: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MDRADMAX_ANH")
val KL_MDRADMAX_BEGR: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MDRADMAX_BEGR")
val KL_MDRADMAX_LMV: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MDRADMAX_LMV")
val KL_MDRADMAX_NOT: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MDRADMAX_NOT")
val KL_PMAX_TGHAS: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_PMAX_TGHAS")
val K_MDGW_MAX: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDGW_MAX")
val K_MDGW_MAX_HS: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDGW_MAX_HS")
val K_MDGW_MAX_LM: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDGW_MAX_LM")
val K_MDGW_MAX_LM_HS: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDGW_MAX_LM_HS")
val K_MDKRADBEGR_NMAX: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDKRADBEGR_NMAX")
val K_MDKRADBEGR_NMAX_GRENZ: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDKRADBEGR_NMAX_GRENZ")
val K_MDRADMAX_LMV: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDRADMAX_LMV")
val K_MDRAD_AW_MAX: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDRAD_AW_MAX")
val K_MDRAD_AW_MAX_HS: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDRAD_AW_MAX_HS")
val K_MDRAD_AW_MAX_LM: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDRAD_AW_MAX_LM")
val K_MDRAD_AW_MAX_LM_HS: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDRAD_AW_MAX_LM_HS")
val K_MDRAD_BEGR_GRD_DN: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDRAD_BEGR_GRD_DN")
val K_MDRAD_BEGR_GRD_UP_ERSATZ: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDRAD_BEGR_GRD_UP_ERSATZ")
val K_MDRAD_MAX_ALLRAD: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDRAD_MAX_ALLRAD")
val K_MDRAD_MAX_ALLRAD_HS: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDRAD_MAX_ALLRAD_HS")
val K_MDRAD_MAX_BEGR_GRD_UP: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDRAD_MAX_BEGR_GRD_UP")
val K_TD_MDBGR_FAHRLEISTUNG: BigDecimal = a2lBin.readCharacteristicWithCast("K_TD_MDBGR_FAHRLEISTUNG")
val K_TGHAS_MAX: BigDecimal = a2lBin.readCharacteristicWithCast("K_TGHAS_MAX")
val S_MDRAD_GRD_UP_ERSATZ: String = a2lBin.readCharacteristicWithCast("S_MDRAD_GRD_UP_ERSATZ")
val BMWtqc_Fac_StatLossHaxl: InMeasurement = a2lBin.measurement("BMWtqc_Fac_StatLossHaxl")
val BMWtqc_Rat_GbxWhlHaxl: InMeasurement = a2lBin.measurement("BMWtqc_Rat_GbxWhlHaxl")
val BMWtqc_Spd_ActLeWhlHaxl: InMeasurement = a2lBin.measurement("BMWtqc_Spd_ActLeWhlHaxl")
val BMWtqc_Spd_ActRiWhlHaxl: InMeasurement = a2lBin.measurement("BMWtqc_Spd_ActRiWhlHaxl")
val BMWtqc_Tqw_StatLossHaxl: InMeasurement = a2lBin.measurement("BMWtqc_Tqw_StatLossHaxl")
val B_leistungsmessung: InMeasurement = a2lBin.measurement("B_leistungsmessung")
val Dmdgsoll_hybachse: InMeasurement = a2lBin.measurement("Dmdgsoll_hybachse")
val I_ha: InMeasurement = a2lBin.measurement("I_ha")
val Md_lmv_v_ist: InMeasurement = a2lBin.measurement("Md_lmv_v_ist")
val Md_lmv_v_soll_plaus: InMeasurement = a2lBin.measurement("Md_lmv_v_soll_plaus")
val Nkw: InMeasurement = a2lBin.measurement("Nkw")
val On_antriebsart_cod: InMeasurement = a2lBin.measurement("On_antriebsart_cod")
val St_MDGWF_01: InMeasurement = a2lBin.measurement("St_MDGWF_01")
val St_MDGWF_01_B_dash_mdgwf: InMeasurement = a2lBin.measurement("St_MDGWF_01.B_dash_mdgwf")
val St_MDGWF_01_B_dmls: InMeasurement = a2lBin.measurement("St_MDGWF_01.B_dmls")
val St_MDGWF_01_B_dp: InMeasurement = a2lBin.measurement("St_MDGWF_01.B_dp")
val St_MDGWF_01_B_ls: InMeasurement = a2lBin.measurement("St_MDGWF_01.B_ls")
val St_MDGWF_01_B_lsd_mdgwf: InMeasurement = a2lBin.measurement("St_MDGWF_01.B_lsd_mdgwf")
val St_MDGWF_01_B_vnull: InMeasurement = a2lBin.measurement("St_MDGWF_01.B_vnull")
val St_mdanfahr_cc: InMeasurement = a2lBin.measurement("St_mdanfahr_cc")
val St_mdanfahr_cc_B_anfahr_nmax_disp_flag: InMeasurement = a2lBin.measurement("St_mdanfahr_cc.B_anfahr_nmax_disp_flag")
val St_mdanfahr_cc_B_rennstart_aktiv: InMeasurement = a2lBin.measurement("St_mdanfahr_cc.B_rennstart_aktiv")
val St_mdar0: InMeasurement = a2lBin.measurement("St_mdar0")
val St_mdar0_B_edp: InMeasurement = a2lBin.measurement("St_mdar0.B_edp")
val St_mdar0_B_elsd: InMeasurement = a2lBin.measurement("St_mdar0.B_elsd")
val St_mdar0_B_lsd: InMeasurement = a2lBin.measurement("St_mdar0.B_lsd")
val St_oz: InMeasurement = a2lBin.measurement("St_oz")
val St_oz_B_anhang: InMeasurement = a2lBin.measurement("St_oz.B_anhang")
val St_wk_plaus: InMeasurement = a2lBin.measurement("St_wk_plaus")
val Var_hs: InMeasurement = a2lBin.measurement("Var_hs")
val W_afs: InMeasurement = a2lBin.measurement("W_afs")
val BMWtqw_tqwi_MaxLimHaxl: OutMeasurement = a2lBin.measurement("BMWtqw_tqwi_MaxLimHaxl")
val BMWtqw_tqwi_MaxLimTReDftl: OutMeasurement = a2lBin.measurement("BMWtqw_tqwi_MaxLimTReDftl")

  BMW_MOD_MaBgr(BMWtqc_Fac_StatLossHaxl, BMWtqc_Rat_GbxWhlHaxl, BMWtqc_Spd_ActLeWhlHaxl, BMWtqc_Spd_ActRiWhlHaxl, BMWtqc_Tqw_StatLossHaxl, B_leistungsmessung, Dmdgsoll_hybachse, I_ha, Md_lmv_v_ist, Md_lmv_v_soll_plaus, Nkw, On_antriebsart_cod, St_MDGWF_01, St_MDGWF_01_B_dash_mdgwf, St_MDGWF_01_B_dmls, St_MDGWF_01_B_dp, St_MDGWF_01_B_ls, St_MDGWF_01_B_lsd_mdgwf, St_MDGWF_01_B_vnull, St_mdanfahr_cc, St_mdanfahr_cc_B_anfahr_nmax_disp_flag, St_mdanfahr_cc_B_rennstart_aktiv, St_mdar0, St_mdar0_B_edp, St_mdar0_B_elsd, St_mdar0_B_lsd, St_oz, St_oz_B_anhang, St_wk_plaus, Var_hs, W_afs, CW_MDRADMAX_NMAX, CW_MDRADMAX_WEL, KL_MDABMAX_LMV, KL_MDRADMAX_ANH, KL_MDRADMAX_BEGR, KL_MDRADMAX_LMV, KL_MDRADMAX_NOT, KL_PMAX_TGHAS, K_MDGW_MAX, K_MDGW_MAX_HS, K_MDGW_MAX_LM, K_MDGW_MAX_LM_HS, K_MDKRADBEGR_NMAX, K_MDKRADBEGR_NMAX_GRENZ, K_MDRADMAX_LMV, K_MDRAD_AW_MAX, K_MDRAD_AW_MAX_HS, K_MDRAD_AW_MAX_LM, K_MDRAD_AW_MAX_LM_HS, K_MDRAD_BEGR_GRD_DN, K_MDRAD_BEGR_GRD_UP_ERSATZ, K_MDRAD_MAX_ALLRAD, K_MDRAD_MAX_ALLRAD_HS, K_MDRAD_MAX_BEGR_GRD_UP, K_TD_MDBGR_FAHRLEISTUNG, K_TGHAS_MAX, S_MDRAD_GRD_UP_ERSATZ, BMWtqw_tqwi_MaxLimHaxl, BMWtqw_tqwi_MaxLimTReDftl)
}


def BMW_MOD_Mafw_Kriech(BMWtqw_tqw_EngDfcoMinStat_sw: InMeasurement, Brtorqsum_plaus: InMeasurement, Fahrstufe_antrieb: InMeasurement, Neig_l_plaus: InMeasurement, Nstat: InMeasurement, Pwg_md_diff: InMeasurement, St_antrieb_wunsch: InMeasurement, St_fw: InMeasurement, St_fw_B_hschalt_komb: InMeasurement, St_fw_B_ldm_akt04: InMeasurement, St_fw_B_ldm_off: InMeasurement, St_fw_B_ldm_offhs: InMeasurement, St_fw_B_ldm_offreg: InMeasurement, St_fw_B_ldm_offrs: InMeasurement, St_fw_B_rschalt_komb: InMeasurement, St_mdipmfw: InMeasurement, St_mdipmfw_B_bst: InMeasurement, St_mdipmfw_B_emf_aktiv: InMeasurement, St_mdipmfw_B_reku: InMeasurement, St_mdipmfw_B_schlepp: InMeasurement, Status_antrieb_ist: InMeasurement, V_fzg_fahrtricht_max: InMeasurement, V_mafw: InMeasurement, Var_hs: InMeasurement, KF_MDBREMS: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MD_KRIECHEN: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MD_KRIECHEN_RUECK: MapType[BigDecimal, BigDecimal, BigDecimal], KL_KRIECH_FILTER: CurveType[BigDecimal, BigDecimal], KL_MD_KRIECHEN_EGS: CurveType[BigDecimal, BigDecimal], KL_MD_KRIECHEN_PWG: CurveType[BigDecimal, BigDecimal], K_EMF_MD_ON: BigDecimal, K_EMF_RAMP_OFF: BigDecimal, K_EMF_RAMP_ON: BigDecimal, K_GB_KRIECH_OFF: BigDecimal, K_GB_KRIECH_ON: BigDecimal, K_NSTAT_FILTER: BigDecimal, K_TD_GB_KRIECH_KS_IST: BigDecimal, K_TD_MAFW_WECHSEL_D_R_D: BigDecimal, K_ZK_ACC_KRIECH: BigDecimal, K_ZK_ACC_KRIECH_RAMP: BigDecimal, Md_rad_brems: OutMeasurement, Md_rad_kriech: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_Mafw_Kriech(a2lBin: A2LBinAdapter): Unit = {

val KF_MDBREMS: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDBREMS")
val KF_MD_KRIECHEN: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MD_KRIECHEN")
val KF_MD_KRIECHEN_RUECK: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MD_KRIECHEN_RUECK")
val KL_KRIECH_FILTER: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_KRIECH_FILTER")
val KL_MD_KRIECHEN_EGS: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MD_KRIECHEN_EGS")
val KL_MD_KRIECHEN_PWG: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MD_KRIECHEN_PWG")
val K_EMF_MD_ON: BigDecimal = a2lBin.readCharacteristicWithCast("K_EMF_MD_ON")
val K_EMF_RAMP_OFF: BigDecimal = a2lBin.readCharacteristicWithCast("K_EMF_RAMP_OFF")
val K_EMF_RAMP_ON: BigDecimal = a2lBin.readCharacteristicWithCast("K_EMF_RAMP_ON")
val K_GB_KRIECH_OFF: BigDecimal = a2lBin.readCharacteristicWithCast("K_GB_KRIECH_OFF")
val K_GB_KRIECH_ON: BigDecimal = a2lBin.readCharacteristicWithCast("K_GB_KRIECH_ON")
val K_NSTAT_FILTER: BigDecimal = a2lBin.readCharacteristicWithCast("K_NSTAT_FILTER")
val K_TD_GB_KRIECH_KS_IST: BigDecimal = a2lBin.readCharacteristicWithCast("K_TD_GB_KRIECH_KS_IST")
val K_TD_MAFW_WECHSEL_D_R_D: BigDecimal = a2lBin.readCharacteristicWithCast("K_TD_MAFW_WECHSEL_D_R_D")
val K_ZK_ACC_KRIECH: BigDecimal = a2lBin.readCharacteristicWithCast("K_ZK_ACC_KRIECH")
val K_ZK_ACC_KRIECH_RAMP: BigDecimal = a2lBin.readCharacteristicWithCast("K_ZK_ACC_KRIECH_RAMP")
val BMWtqw_tqw_EngDfcoMinStat_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_EngDfcoMinStat_sw")
val Brtorqsum_plaus: InMeasurement = a2lBin.measurement("Brtorqsum_plaus")
val Fahrstufe_antrieb: InMeasurement = a2lBin.measurement("Fahrstufe_antrieb")
val Neig_l_plaus: InMeasurement = a2lBin.measurement("Neig_l_plaus")
val Nstat: InMeasurement = a2lBin.measurement("Nstat")
val Pwg_md_diff: InMeasurement = a2lBin.measurement("Pwg_md_diff")
val St_antrieb_wunsch: InMeasurement = a2lBin.measurement("St_antrieb_wunsch")
val St_fw: InMeasurement = a2lBin.measurement("St_fw")
val St_fw_B_hschalt_komb: InMeasurement = a2lBin.measurement("St_fw.B_hschalt_komb")
val St_fw_B_ldm_akt04: InMeasurement = a2lBin.measurement("St_fw.B_ldm_akt04")
val St_fw_B_ldm_off: InMeasurement = a2lBin.measurement("St_fw.B_ldm_off")
val St_fw_B_ldm_offhs: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offhs")
val St_fw_B_ldm_offreg: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offreg")
val St_fw_B_ldm_offrs: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offrs")
val St_fw_B_rschalt_komb: InMeasurement = a2lBin.measurement("St_fw.B_rschalt_komb")
val St_mdipmfw: InMeasurement = a2lBin.measurement("St_mdipmfw")
val St_mdipmfw_B_bst: InMeasurement = a2lBin.measurement("St_mdipmfw.B_bst")
val St_mdipmfw_B_emf_aktiv: InMeasurement = a2lBin.measurement("St_mdipmfw.B_emf_aktiv")
val St_mdipmfw_B_reku: InMeasurement = a2lBin.measurement("St_mdipmfw.B_reku")
val St_mdipmfw_B_schlepp: InMeasurement = a2lBin.measurement("St_mdipmfw.B_schlepp")
val Status_antrieb_ist: InMeasurement = a2lBin.measurement("Status_antrieb_ist")
val V_fzg_fahrtricht_max: InMeasurement = a2lBin.measurement("V_fzg_fahrtricht_max")
val V_mafw: InMeasurement = a2lBin.measurement("V_mafw")
val Var_hs: InMeasurement = a2lBin.measurement("Var_hs")
val Md_rad_brems: OutMeasurement = a2lBin.measurement("Md_rad_brems")
val Md_rad_kriech: OutMeasurement = a2lBin.measurement("Md_rad_kriech")

  BMW_MOD_Mafw_Kriech(BMWtqw_tqw_EngDfcoMinStat_sw, Brtorqsum_plaus, Fahrstufe_antrieb, Neig_l_plaus, Nstat, Pwg_md_diff, St_antrieb_wunsch, St_fw, St_fw_B_hschalt_komb, St_fw_B_ldm_akt04, St_fw_B_ldm_off, St_fw_B_ldm_offhs, St_fw_B_ldm_offreg, St_fw_B_ldm_offrs, St_fw_B_rschalt_komb, St_mdipmfw, St_mdipmfw_B_bst, St_mdipmfw_B_emf_aktiv, St_mdipmfw_B_reku, St_mdipmfw_B_schlepp, Status_antrieb_ist, V_fzg_fahrtricht_max, V_mafw, Var_hs, KF_MDBREMS, KF_MD_KRIECHEN, KF_MD_KRIECHEN_RUECK, KL_KRIECH_FILTER, KL_MD_KRIECHEN_EGS, KL_MD_KRIECHEN_PWG, K_EMF_MD_ON, K_EMF_RAMP_OFF, K_EMF_RAMP_ON, K_GB_KRIECH_OFF, K_GB_KRIECH_ON, K_NSTAT_FILTER, K_TD_GB_KRIECH_KS_IST, K_TD_MAFW_WECHSEL_D_R_D, K_ZK_ACC_KRIECH, K_ZK_ACC_KRIECH_RAMP, Md_rad_brems, Md_rad_kriech)
}


def BMW_MOD_Mafw_MdMax(BMWtqc_Rat_OptmGbxWhlHaxl: InMeasurement, F_rad_statverl: InMeasurement, Md_rad_statverl: InMeasurement, Nkw_opt: InMeasurement, Pwg_ist_mafw: InMeasurement, St_kickdown: InMeasurement, St_kickdown_B_kickdown: InMeasurement, St_mdipmfw: InMeasurement, St_mdipmfw_B_bst: InMeasurement, St_mdipmfw_B_emf_aktiv: InMeasurement, St_mdipmfw_B_reku: InMeasurement, St_mdipmfw_B_schlepp: InMeasurement, Status_usecase_antr_lim: InMeasurement, V_fzg_plaus: InMeasurement, V_mafw: InMeasurement, Var_hs: InMeasurement, Zka_fak: InMeasurement, KL_MDR_DIFF_WLC_MAFW: CurveType[BigDecimal, BigDecimal], KL_MD_K_MAX_BST: CurveType[BigDecimal, BigDecimal], KL_MD_K_MAX_BST_HS: CurveType[BigDecimal, BigDecimal], KL_MD_K_MAX_RST: CurveType[BigDecimal, BigDecimal], KL_MD_K_MAX_RST_HS: CurveType[BigDecimal, BigDecimal], KL_MD_K_MAX_VL: CurveType[BigDecimal, BigDecimal], KL_MD_K_MAX_VL_HS: CurveType[BigDecimal, BigDecimal], KL_MD_RAD_MAX_BST: CurveType[BigDecimal, BigDecimal], KL_MD_RAD_MAX_RST: CurveType[BigDecimal, BigDecimal], KL_MD_RAD_MAX_VL: CurveType[BigDecimal, BigDecimal], KL_UC_BOOST: CurveType[BigDecimal, BigDecimal], K_BST_OFF: BigDecimal, K_BST_ON: BigDecimal, K_KICK_MAX: BigDecimal, K_MDR_VERL_MAFW: BigDecimal, K_PWG_BST: BigDecimal, S_SPT104_CP458784: String, Mafw_mmax_fader: OutMeasurement, Md_rad_max_rk: OutMeasurement, Md_rad_max_zka: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_Mafw_MdMax(a2lBin: A2LBinAdapter): Unit = {

val KL_MDR_DIFF_WLC_MAFW: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MDR_DIFF_WLC_MAFW")
val KL_MD_K_MAX_BST: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MD_K_MAX_BST")
val KL_MD_K_MAX_BST_HS: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MD_K_MAX_BST_HS")
val KL_MD_K_MAX_RST: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MD_K_MAX_RST")
val KL_MD_K_MAX_RST_HS: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MD_K_MAX_RST_HS")
val KL_MD_K_MAX_VL: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MD_K_MAX_VL")
val KL_MD_K_MAX_VL_HS: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MD_K_MAX_VL_HS")
val KL_MD_RAD_MAX_BST: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MD_RAD_MAX_BST")
val KL_MD_RAD_MAX_RST: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MD_RAD_MAX_RST")
val KL_MD_RAD_MAX_VL: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MD_RAD_MAX_VL")
val KL_UC_BOOST: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_UC_BOOST")
val K_BST_OFF: BigDecimal = a2lBin.readCharacteristicWithCast("K_BST_OFF")
val K_BST_ON: BigDecimal = a2lBin.readCharacteristicWithCast("K_BST_ON")
val K_KICK_MAX: BigDecimal = a2lBin.readCharacteristicWithCast("K_KICK_MAX")
val K_MDR_VERL_MAFW: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDR_VERL_MAFW")
val K_PWG_BST: BigDecimal = a2lBin.readCharacteristicWithCast("K_PWG_BST")
val S_SPT104_CP458784: String = a2lBin.readCharacteristicWithCast("S_SPT104_CP458784")
val BMWtqc_Rat_OptmGbxWhlHaxl: InMeasurement = a2lBin.measurement("BMWtqc_Rat_OptmGbxWhlHaxl")
val F_rad_statverl: InMeasurement = a2lBin.measurement("F_rad_statverl")
val Md_rad_statverl: InMeasurement = a2lBin.measurement("Md_rad_statverl")
val Nkw_opt: InMeasurement = a2lBin.measurement("Nkw_opt")
val Pwg_ist_mafw: InMeasurement = a2lBin.measurement("Pwg_ist_mafw")
val St_kickdown: InMeasurement = a2lBin.measurement("St_kickdown")
val St_kickdown_B_kickdown: InMeasurement = a2lBin.measurement("St_kickdown.B_kickdown")
val St_mdipmfw: InMeasurement = a2lBin.measurement("St_mdipmfw")
val St_mdipmfw_B_bst: InMeasurement = a2lBin.measurement("St_mdipmfw.B_bst")
val St_mdipmfw_B_emf_aktiv: InMeasurement = a2lBin.measurement("St_mdipmfw.B_emf_aktiv")
val St_mdipmfw_B_reku: InMeasurement = a2lBin.measurement("St_mdipmfw.B_reku")
val St_mdipmfw_B_schlepp: InMeasurement = a2lBin.measurement("St_mdipmfw.B_schlepp")
val Status_usecase_antr_lim: InMeasurement = a2lBin.measurement("Status_usecase_antr_lim")
val V_fzg_plaus: InMeasurement = a2lBin.measurement("V_fzg_plaus")
val V_mafw: InMeasurement = a2lBin.measurement("V_mafw")
val Var_hs: InMeasurement = a2lBin.measurement("Var_hs")
val Zka_fak: InMeasurement = a2lBin.measurement("Zka_fak")
val Mafw_mmax_fader: OutMeasurement = a2lBin.measurement("Mafw_mmax_fader")
val Md_rad_max_rk: OutMeasurement = a2lBin.measurement("Md_rad_max_rk")
val Md_rad_max_zka: OutMeasurement = a2lBin.measurement("Md_rad_max_zka")

  BMW_MOD_Mafw_MdMax(BMWtqc_Rat_OptmGbxWhlHaxl, F_rad_statverl, Md_rad_statverl, Nkw_opt, Pwg_ist_mafw, St_kickdown, St_kickdown_B_kickdown, St_mdipmfw, St_mdipmfw_B_bst, St_mdipmfw_B_emf_aktiv, St_mdipmfw_B_reku, St_mdipmfw_B_schlepp, Status_usecase_antr_lim, V_fzg_plaus, V_mafw, Var_hs, Zka_fak, KL_MDR_DIFF_WLC_MAFW, KL_MD_K_MAX_BST, KL_MD_K_MAX_BST_HS, KL_MD_K_MAX_RST, KL_MD_K_MAX_RST_HS, KL_MD_K_MAX_VL, KL_MD_K_MAX_VL_HS, KL_MD_RAD_MAX_BST, KL_MD_RAD_MAX_RST, KL_MD_RAD_MAX_VL, KL_UC_BOOST, K_BST_OFF, K_BST_ON, K_KICK_MAX, K_MDR_VERL_MAFW, K_PWG_BST, S_SPT104_CP458784, Mafw_mmax_fader, Md_rad_max_rk, Md_rad_max_zka)
}


def BMW_MOD_Mafw_MdMin(BMWosc_tqw_TarCnctRecu_sw: InMeasurement, Md_rad_brems_antr_soll: InMeasurement, Md_rad_rekupmx: InMeasurement, Md_rad_rueck: InMeasurement, Md_rad_schlepp: InMeasurement, Md_rad_schlepp_max: InMeasurement, Md_rad_schlepp_od: InMeasurement, Md_rad_schlepp_soll_roh: InMeasurement, Md_rad_steig_schub: InMeasurement, Pwg_ist_mafw: InMeasurement, Pwg_md_rad_null: InMeasurement, St_mdipmfw: InMeasurement, St_mdipmfw_B_bst: InMeasurement, St_mdipmfw_B_emf_aktiv: InMeasurement, St_mdipmfw_B_reku: InMeasurement, St_mdipmfw_B_schlepp: InMeasurement, St_mdrdmk: InMeasurement, St_mdrdmk_B_fas_dcc: InMeasurement, St_mdrdmk_B_fas_sld: InMeasurement, St_uc_schlepp: InMeasurement, Status_usecase_antr_lim: InMeasurement, V_mafw: InMeasurement, CW_MD_RAD_SCHLEPP_MGASSE: BigDecimal, KF_STEIGKOMP_REKU_PROG: MapType[BigDecimal, BigDecimal, BigDecimal], KL_MD_RAD_MIN: CurveType[BigDecimal, BigDecimal], KL_STEIGKOMP_REKU_QUOT: CurveType[BigDecimal, BigDecimal], K_F_GRD_DN_BREMS_STEIG_ENTRY: BigDecimal, K_MD_RAD_SCHLEPP_ANF_UC_GRDDN: BigDecimal, K_MD_RAD_SCHLEPP_ANF_UC_GRDUP: BigDecimal, K_MD_RAD_SCHLEPP_ANF_UC_M_GRDDN: BigDecimal, K_MD_RAD_SCHLEPP_ANF_UC_M_GRDUP: BigDecimal, K_MD_RAD_STEIG_REKUP_EIN: BigDecimal, K_MD_RAD_STEIG_SCHUB_EIN: BigDecimal, K_REKUP_VERZ: BigDecimal, K_TD_SCHLEPP_M_GASSE: BigDecimal, S_MAFW_BREMSREKU_OHNE_DYNAMIK: String, S_MAFW_REKUEINSTIEG: String, Md_rad_min_zka: OutMeasurement, Md_rad_schlepp_soll: OutMeasurement, Md_rad_schlepp_soll_diff: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_Mafw_MdMin(a2lBin: A2LBinAdapter): Unit = {

val CW_MD_RAD_SCHLEPP_MGASSE: BigDecimal = a2lBin.readCharacteristicWithCast("CW_MD_RAD_SCHLEPP_MGASSE")
val KF_STEIGKOMP_REKU_PROG: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_STEIGKOMP_REKU_PROG")
val KL_MD_RAD_MIN: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MD_RAD_MIN")
val KL_STEIGKOMP_REKU_QUOT: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_STEIGKOMP_REKU_QUOT")
val K_F_GRD_DN_BREMS_STEIG_ENTRY: BigDecimal = a2lBin.readCharacteristicWithCast("K_F_GRD_DN_BREMS_STEIG_ENTRY")
val K_MD_RAD_SCHLEPP_ANF_UC_GRDDN: BigDecimal = a2lBin.readCharacteristicWithCast("K_MD_RAD_SCHLEPP_ANF_UC_GRDDN")
val K_MD_RAD_SCHLEPP_ANF_UC_GRDUP: BigDecimal = a2lBin.readCharacteristicWithCast("K_MD_RAD_SCHLEPP_ANF_UC_GRDUP")
val K_MD_RAD_SCHLEPP_ANF_UC_M_GRDDN: BigDecimal = a2lBin.readCharacteristicWithCast("K_MD_RAD_SCHLEPP_ANF_UC_M_GRDDN")
val K_MD_RAD_SCHLEPP_ANF_UC_M_GRDUP: BigDecimal = a2lBin.readCharacteristicWithCast("K_MD_RAD_SCHLEPP_ANF_UC_M_GRDUP")
val K_MD_RAD_STEIG_REKUP_EIN: BigDecimal = a2lBin.readCharacteristicWithCast("K_MD_RAD_STEIG_REKUP_EIN")
val K_MD_RAD_STEIG_SCHUB_EIN: BigDecimal = a2lBin.readCharacteristicWithCast("K_MD_RAD_STEIG_SCHUB_EIN")
val K_REKUP_VERZ: BigDecimal = a2lBin.readCharacteristicWithCast("K_REKUP_VERZ")
val K_TD_SCHLEPP_M_GASSE: BigDecimal = a2lBin.readCharacteristicWithCast("K_TD_SCHLEPP_M_GASSE")
val S_MAFW_BREMSREKU_OHNE_DYNAMIK: String = a2lBin.readCharacteristicWithCast("S_MAFW_BREMSREKU_OHNE_DYNAMIK")
val S_MAFW_REKUEINSTIEG: String = a2lBin.readCharacteristicWithCast("S_MAFW_REKUEINSTIEG")
val BMWosc_tqw_TarCnctRecu_sw: InMeasurement = a2lBin.measurement("BMWosc_tqw_TarCnctRecu_sw")
val Md_rad_brems_antr_soll: InMeasurement = a2lBin.measurement("Md_rad_brems_antr_soll")
val Md_rad_rekupmx: InMeasurement = a2lBin.measurement("Md_rad_rekupmx")
val Md_rad_rueck: InMeasurement = a2lBin.measurement("Md_rad_rueck")
val Md_rad_schlepp: InMeasurement = a2lBin.measurement("Md_rad_schlepp")
val Md_rad_schlepp_max: InMeasurement = a2lBin.measurement("Md_rad_schlepp_max")
val Md_rad_schlepp_od: InMeasurement = a2lBin.measurement("Md_rad_schlepp_od")
val Md_rad_schlepp_soll_roh: InMeasurement = a2lBin.measurement("Md_rad_schlepp_soll_roh")
val Md_rad_steig_schub: InMeasurement = a2lBin.measurement("Md_rad_steig_schub")
val Pwg_ist_mafw: InMeasurement = a2lBin.measurement("Pwg_ist_mafw")
val Pwg_md_rad_null: InMeasurement = a2lBin.measurement("Pwg_md_rad_null")
val St_mdipmfw: InMeasurement = a2lBin.measurement("St_mdipmfw")
val St_mdipmfw_B_bst: InMeasurement = a2lBin.measurement("St_mdipmfw.B_bst")
val St_mdipmfw_B_emf_aktiv: InMeasurement = a2lBin.measurement("St_mdipmfw.B_emf_aktiv")
val St_mdipmfw_B_reku: InMeasurement = a2lBin.measurement("St_mdipmfw.B_reku")
val St_mdipmfw_B_schlepp: InMeasurement = a2lBin.measurement("St_mdipmfw.B_schlepp")
val St_mdrdmk: InMeasurement = a2lBin.measurement("St_mdrdmk")
val St_mdrdmk_B_fas_dcc: InMeasurement = a2lBin.measurement("St_mdrdmk.B_fas_dcc")
val St_mdrdmk_B_fas_sld: InMeasurement = a2lBin.measurement("St_mdrdmk.B_fas_sld")
val St_uc_schlepp: InMeasurement = a2lBin.measurement("St_uc_schlepp")
val Status_usecase_antr_lim: InMeasurement = a2lBin.measurement("Status_usecase_antr_lim")
val V_mafw: InMeasurement = a2lBin.measurement("V_mafw")
val Md_rad_min_zka: OutMeasurement = a2lBin.measurement("Md_rad_min_zka")
val Md_rad_schlepp_soll: OutMeasurement = a2lBin.measurement("Md_rad_schlepp_soll")
val Md_rad_schlepp_soll_diff: OutMeasurement = a2lBin.measurement("Md_rad_schlepp_soll_diff")

  BMW_MOD_Mafw_MdMin(BMWosc_tqw_TarCnctRecu_sw, Md_rad_brems_antr_soll, Md_rad_rekupmx, Md_rad_rueck, Md_rad_schlepp, Md_rad_schlepp_max, Md_rad_schlepp_od, Md_rad_schlepp_soll_roh, Md_rad_steig_schub, Pwg_ist_mafw, Pwg_md_rad_null, St_mdipmfw, St_mdipmfw_B_bst, St_mdipmfw_B_emf_aktiv, St_mdipmfw_B_reku, St_mdipmfw_B_schlepp, St_mdrdmk, St_mdrdmk_B_fas_dcc, St_mdrdmk_B_fas_sld, St_uc_schlepp, Status_usecase_antr_lim, V_mafw, CW_MD_RAD_SCHLEPP_MGASSE, KF_STEIGKOMP_REKU_PROG, KL_MD_RAD_MIN, KL_STEIGKOMP_REKU_QUOT, K_F_GRD_DN_BREMS_STEIG_ENTRY, K_MD_RAD_SCHLEPP_ANF_UC_GRDDN, K_MD_RAD_SCHLEPP_ANF_UC_GRDUP, K_MD_RAD_SCHLEPP_ANF_UC_M_GRDDN, K_MD_RAD_SCHLEPP_ANF_UC_M_GRDUP, K_MD_RAD_STEIG_REKUP_EIN, K_MD_RAD_STEIG_SCHUB_EIN, K_REKUP_VERZ, K_TD_SCHLEPP_M_GASSE, S_MAFW_BREMSREKU_OHNE_DYNAMIK, S_MAFW_REKUEINSTIEG, Md_rad_min_zka, Md_rad_schlepp_soll, Md_rad_schlepp_soll_diff)
}


def BMW_MOD_Mafw_Pedal(I_ges: InMeasurement, Md_rad_max_zka: InMeasurement, Md_rad_min_zka: InMeasurement, Nkw_zkafak: InMeasurement, Pwg_ist_mafw: InMeasurement, Status_usecase_antr_lim: InMeasurement, V_mafw: InMeasurement, Var_hs: InMeasurement, KF_MDR_ABV_D: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MDR_ABV_D_HS: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MDR_ABV_D_S: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MDR_ABV_ECO: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MDR_ABV_ECO2: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MDR_ABV_ECO2_HS: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MDR_ABV_ECO_HS: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MDR_ABV_ECO_S: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MDR_ABV_SPO: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MDR_ABV_SPO2: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MDR_ABV_SPO2_HS: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MDR_ABV_SPO_HS: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MDR_ABV_SPO_S: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MD_RAD_NULL_NKW: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MD_RAD_NULL_NKW_HS: MapType[BigDecimal, BigDecimal, BigDecimal], KL_MDABNULL_FAK: CurveType[BigDecimal, BigDecimal], KL_MD_RAD_NULL: CurveType[BigDecimal, BigDecimal], KL_MD_RAD_NULL_SPO: CurveType[BigDecimal, BigDecimal], KL_UC_NULLLINIE: CurveType[BigDecimal, BigDecimal], KL_UC_PEDALPROGRESS: CurveType[BigDecimal, BigDecimal], KL_UC_PEDALRAMPE: CurveType[BigDecimal, BigDecimal], K_MDR_ABV_SPREIZ: BigDecimal, K_PWG_RAMPE_MIN: BigDecimal, S_PWG_RAMPE: String, K_M_SW_VLIM: BigDecimal, K_R_SW_VLIM: BigDecimal, Mdr_pedal_neg: OutMeasurement, Mdr_pedal_pos: OutMeasurement, Pwg_fvv_fakipm: OutMeasurement, Pwg_md_diff: OutMeasurement, Pwg_md_rad_null: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_Mafw_Pedal(a2lBin: A2LBinAdapter): Unit = {

val KF_MDR_ABV_D: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDR_ABV_D")
val KF_MDR_ABV_D_HS: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDR_ABV_D_HS")
val KF_MDR_ABV_D_S: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDR_ABV_D_S")
val KF_MDR_ABV_ECO: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDR_ABV_ECO")
val KF_MDR_ABV_ECO2: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDR_ABV_ECO2")
val KF_MDR_ABV_ECO2_HS: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDR_ABV_ECO2_HS")
val KF_MDR_ABV_ECO_HS: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDR_ABV_ECO_HS")
val KF_MDR_ABV_ECO_S: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDR_ABV_ECO_S")
val KF_MDR_ABV_SPO: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDR_ABV_SPO")
val KF_MDR_ABV_SPO2: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDR_ABV_SPO2")
val KF_MDR_ABV_SPO2_HS: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDR_ABV_SPO2_HS")
val KF_MDR_ABV_SPO_HS: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDR_ABV_SPO_HS")
val KF_MDR_ABV_SPO_S: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDR_ABV_SPO_S")
val KF_MD_RAD_NULL_NKW: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MD_RAD_NULL_NKW")
val KF_MD_RAD_NULL_NKW_HS: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MD_RAD_NULL_NKW_HS")
val KL_MDABNULL_FAK: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MDABNULL_FAK")
val KL_MD_RAD_NULL: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MD_RAD_NULL")
val KL_MD_RAD_NULL_SPO: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MD_RAD_NULL_SPO")
val KL_UC_NULLLINIE: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_UC_NULLLINIE")
val KL_UC_PEDALPROGRESS: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_UC_PEDALPROGRESS")
val KL_UC_PEDALRAMPE: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_UC_PEDALRAMPE")
val K_MDR_ABV_SPREIZ: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDR_ABV_SPREIZ")
val K_PWG_RAMPE_MIN: BigDecimal = a2lBin.readCharacteristicWithCast("K_PWG_RAMPE_MIN")
val S_PWG_RAMPE: String = a2lBin.readCharacteristicWithCast("S_PWG_RAMPE")
val K_M_SW_VLIM: BigDecimal = a2lBin.readCharacteristicWithCast("K_M_SW_VLIM")
val K_R_SW_VLIM: BigDecimal = a2lBin.readCharacteristicWithCast("K_R_SW_VLIM")
val I_ges: InMeasurement = a2lBin.measurement("I_ges")
val Md_rad_max_zka: InMeasurement = a2lBin.measurement("Md_rad_max_zka")
val Md_rad_min_zka: InMeasurement = a2lBin.measurement("Md_rad_min_zka")
val Nkw_zkafak: InMeasurement = a2lBin.measurement("Nkw_zkafak")
val Pwg_ist_mafw: InMeasurement = a2lBin.measurement("Pwg_ist_mafw")
val Status_usecase_antr_lim: InMeasurement = a2lBin.measurement("Status_usecase_antr_lim")
val V_mafw: InMeasurement = a2lBin.measurement("V_mafw")
val Var_hs: InMeasurement = a2lBin.measurement("Var_hs")
val Mdr_pedal_neg: OutMeasurement = a2lBin.measurement("Mdr_pedal_neg")
val Mdr_pedal_pos: OutMeasurement = a2lBin.measurement("Mdr_pedal_pos")
val Pwg_fvv_fakipm: OutMeasurement = a2lBin.measurement("Pwg_fvv_fakipm")
val Pwg_md_diff: OutMeasurement = a2lBin.measurement("Pwg_md_diff")
val Pwg_md_rad_null: OutMeasurement = a2lBin.measurement("Pwg_md_rad_null")

  BMW_MOD_Mafw_Pedal(I_ges, Md_rad_max_zka, Md_rad_min_zka, Nkw_zkafak, Pwg_ist_mafw, Status_usecase_antr_lim, V_mafw, Var_hs, KF_MDR_ABV_D, KF_MDR_ABV_D_HS, KF_MDR_ABV_D_S, KF_MDR_ABV_ECO, KF_MDR_ABV_ECO2, KF_MDR_ABV_ECO2_HS, KF_MDR_ABV_ECO_HS, KF_MDR_ABV_ECO_S, KF_MDR_ABV_SPO, KF_MDR_ABV_SPO2, KF_MDR_ABV_SPO2_HS, KF_MDR_ABV_SPO_HS, KF_MDR_ABV_SPO_S, KF_MD_RAD_NULL_NKW, KF_MD_RAD_NULL_NKW_HS, KL_MDABNULL_FAK, KL_MD_RAD_NULL, KL_MD_RAD_NULL_SPO, KL_UC_NULLLINIE, KL_UC_PEDALPROGRESS, KL_UC_PEDALRAMPE, K_MDR_ABV_SPREIZ, K_PWG_RAMPE_MIN, S_PWG_RAMPE, K_M_SW_VLIM, K_R_SW_VLIM, Mdr_pedal_neg, Mdr_pedal_pos, Pwg_fvv_fakipm, Pwg_md_diff, Pwg_md_rad_null)
}


def BMW_MOD_Mafw_PreCond(I_ges: InMeasurement, Msa_stfzg: InMeasurement, Msa_stfzgpbr: InMeasurement, Nkw_ref: InMeasurement, Nkw_zkafak: InMeasurement, Nstat: InMeasurement, Pwg_ist: InMeasurement, St_kickdown: InMeasurement, St_kickdown_B_kickdown: InMeasurement, St_mdrdmk: InMeasurement, St_mdrdmk_B_fas_dcc: InMeasurement, St_mdrdmk_B_fas_sld: InMeasurement, St_q_rekup_anf_plaus: InMeasurement, Status_usecase_antr: InMeasurement, Status_usecase_mafw: InMeasurement, V_ersatz_n_gb1_ab: InMeasurement, V_ersatz_n_gb1_ab_q: InMeasurement, V_fzg_achse_max: InMeasurement, V_fzg_achse_max_q: InMeasurement, V_fzg_fahrtricht: InMeasurement, V_fzg_fahrtricht_tqw: InMeasurement, Var_at: InMeasurement, KL_NKWPLAUS_AKT: CurveType[BigDecimal, BigDecimal], KL_NKWPLAUS_NSTAT: CurveType[BigDecimal, BigDecimal], KL_NKWPLAUS_OFFSET: CurveType[BigDecimal, BigDecimal], KL_PWG_IST_FILTER: CurveType[BigDecimal, BigDecimal], K_AKTIV_Q_LIM_REKUP: BigDecimal, K_AKTIV_Q_REKUP: BigDecimal, K_MASK_SOLL_Q_REKUP: BigDecimal, K_MDRADBEGR_EMF_ERR: BigDecimal, K_MDRADBEGR_EMF_MAX: BigDecimal, K_PWG_IST_KICK_OFFSET: BigDecimal, K_PWG_IST_MAFW_HYS_OFF: BigDecimal, K_PWG_IST_MAFW_HYS_ON: BigDecimal, K_PWG_IST_MAFW_MITTEL: BigDecimal, K_V2NRAD_NKW: BigDecimal, S_AUSW_V_MAFW: BigDecimal, S_AUSW_V_MAFW_NL: BigDecimal, S_FID_REKU_AKTIV: BigDecimal, S_MAFW_USE_NEUE_EMF_LOGIK: String, S_MAFW_USE_NEUE_EMF_LOGIK2: String, S_NKW_ZKAFAK: String, S_PWG_IST_MAFW_FILTER: BigDecimal, S_STATUS_USECASE: String, Md_rad_max_emf: OutMeasurement, Pwg_grad: OutMeasurement, Pwg_ist_mafw: OutMeasurement, St_mdipmfw: OutMeasurement, St_mdipmfw_B_bst: OutMeasurement, St_mdipmfw_B_emf_aktiv: OutMeasurement, St_mdipmfw_B_reku: OutMeasurement, St_mdipmfw_B_schlepp: OutMeasurement, Status_usecase_antr_lim: OutMeasurement, V_mafw: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_Mafw_PreCond(a2lBin: A2LBinAdapter): Unit = {

val KL_NKWPLAUS_AKT: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_NKWPLAUS_AKT")
val KL_NKWPLAUS_NSTAT: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_NKWPLAUS_NSTAT")
val KL_NKWPLAUS_OFFSET: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_NKWPLAUS_OFFSET")
val KL_PWG_IST_FILTER: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_PWG_IST_FILTER")
val K_AKTIV_Q_LIM_REKUP: BigDecimal = a2lBin.readCharacteristicWithCast("K_AKTIV_Q_LIM_REKUP")
val K_AKTIV_Q_REKUP: BigDecimal = a2lBin.readCharacteristicWithCast("K_AKTIV_Q_REKUP")
val K_MASK_SOLL_Q_REKUP: BigDecimal = a2lBin.readCharacteristicWithCast("K_MASK_SOLL_Q_REKUP")
val K_MDRADBEGR_EMF_ERR: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDRADBEGR_EMF_ERR")
val K_MDRADBEGR_EMF_MAX: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDRADBEGR_EMF_MAX")
val K_PWG_IST_KICK_OFFSET: BigDecimal = a2lBin.readCharacteristicWithCast("K_PWG_IST_KICK_OFFSET")
val K_PWG_IST_MAFW_HYS_OFF: BigDecimal = a2lBin.readCharacteristicWithCast("K_PWG_IST_MAFW_HYS_OFF")
val K_PWG_IST_MAFW_HYS_ON: BigDecimal = a2lBin.readCharacteristicWithCast("K_PWG_IST_MAFW_HYS_ON")
val K_PWG_IST_MAFW_MITTEL: BigDecimal = a2lBin.readCharacteristicWithCast("K_PWG_IST_MAFW_MITTEL")
val K_V2NRAD_NKW: BigDecimal = a2lBin.readCharacteristicWithCast("K_V2NRAD_NKW")
val S_AUSW_V_MAFW: BigDecimal = a2lBin.readCharacteristicWithCast("S_AUSW_V_MAFW")
val S_AUSW_V_MAFW_NL: BigDecimal = a2lBin.readCharacteristicWithCast("S_AUSW_V_MAFW_NL")
val S_FID_REKU_AKTIV: BigDecimal = a2lBin.readCharacteristicWithCast("S_FID_REKU_AKTIV")
val S_MAFW_USE_NEUE_EMF_LOGIK: String = a2lBin.readCharacteristicWithCast("S_MAFW_USE_NEUE_EMF_LOGIK")
val S_MAFW_USE_NEUE_EMF_LOGIK2: String = a2lBin.readCharacteristicWithCast("S_MAFW_USE_NEUE_EMF_LOGIK2")
val S_NKW_ZKAFAK: String = a2lBin.readCharacteristicWithCast("S_NKW_ZKAFAK")
val S_PWG_IST_MAFW_FILTER: BigDecimal = a2lBin.readCharacteristicWithCast("S_PWG_IST_MAFW_FILTER")
val S_STATUS_USECASE: String = a2lBin.readCharacteristicWithCast("S_STATUS_USECASE")
val I_ges: InMeasurement = a2lBin.measurement("I_ges")
val Msa_stfzg: InMeasurement = a2lBin.measurement("Msa_stfzg")
val Msa_stfzgpbr: InMeasurement = a2lBin.measurement("Msa_stfzgpbr")
val Nkw_ref: InMeasurement = a2lBin.measurement("Nkw_ref")
val Nkw_zkafak: InMeasurement = a2lBin.measurement("Nkw_zkafak")
val Nstat: InMeasurement = a2lBin.measurement("Nstat")
val Pwg_ist: InMeasurement = a2lBin.measurement("Pwg_ist")
val St_kickdown: InMeasurement = a2lBin.measurement("St_kickdown")
val St_kickdown_B_kickdown: InMeasurement = a2lBin.measurement("St_kickdown.B_kickdown")
val St_mdrdmk: InMeasurement = a2lBin.measurement("St_mdrdmk")
val St_mdrdmk_B_fas_dcc: InMeasurement = a2lBin.measurement("St_mdrdmk.B_fas_dcc")
val St_mdrdmk_B_fas_sld: InMeasurement = a2lBin.measurement("St_mdrdmk.B_fas_sld")
val St_q_rekup_anf_plaus: InMeasurement = a2lBin.measurement("St_q_rekup_anf_plaus")
val Status_usecase_antr: InMeasurement = a2lBin.measurement("Status_usecase_antr")
val Status_usecase_mafw: InMeasurement = a2lBin.measurement("Status_usecase_mafw")
val V_ersatz_n_gb1_ab: InMeasurement = a2lBin.measurement("V_ersatz_n_gb1_ab")
val V_ersatz_n_gb1_ab_q: InMeasurement = a2lBin.measurement("V_ersatz_n_gb1_ab_q")
val V_fzg_achse_max: InMeasurement = a2lBin.measurement("V_fzg_achse_max")
val V_fzg_achse_max_q: InMeasurement = a2lBin.measurement("V_fzg_achse_max_q")
val V_fzg_fahrtricht: InMeasurement = a2lBin.measurement("V_fzg_fahrtricht")
val V_fzg_fahrtricht_tqw: InMeasurement = a2lBin.measurement("V_fzg_fahrtricht_tqw")
val Var_at: InMeasurement = a2lBin.measurement("Var_at")
val Md_rad_max_emf: OutMeasurement = a2lBin.measurement("Md_rad_max_emf")
val Pwg_grad: OutMeasurement = a2lBin.measurement("Pwg_grad")
val Pwg_ist_mafw: OutMeasurement = a2lBin.measurement("Pwg_ist_mafw")
val St_mdipmfw: OutMeasurement = a2lBin.measurement("St_mdipmfw")
val St_mdipmfw_B_bst: OutMeasurement = a2lBin.measurement("St_mdipmfw.B_bst")
val St_mdipmfw_B_emf_aktiv: OutMeasurement = a2lBin.measurement("St_mdipmfw.B_emf_aktiv")
val St_mdipmfw_B_reku: OutMeasurement = a2lBin.measurement("St_mdipmfw.B_reku")
val St_mdipmfw_B_schlepp: OutMeasurement = a2lBin.measurement("St_mdipmfw.B_schlepp")
val Status_usecase_antr_lim: OutMeasurement = a2lBin.measurement("Status_usecase_antr_lim")
val V_mafw: OutMeasurement = a2lBin.measurement("V_mafw")

  BMW_MOD_Mafw_PreCond(I_ges, Msa_stfzg, Msa_stfzgpbr, Nkw_ref, Nkw_zkafak, Nstat, Pwg_ist, St_kickdown, St_kickdown_B_kickdown, St_mdrdmk, St_mdrdmk_B_fas_dcc, St_mdrdmk_B_fas_sld, St_q_rekup_anf_plaus, Status_usecase_antr, Status_usecase_mafw, V_ersatz_n_gb1_ab, V_ersatz_n_gb1_ab_q, V_fzg_achse_max, V_fzg_achse_max_q, V_fzg_fahrtricht, V_fzg_fahrtricht_tqw, Var_at, KL_NKWPLAUS_AKT, KL_NKWPLAUS_NSTAT, KL_NKWPLAUS_OFFSET, KL_PWG_IST_FILTER, K_AKTIV_Q_LIM_REKUP, K_AKTIV_Q_REKUP, K_MASK_SOLL_Q_REKUP, K_MDRADBEGR_EMF_ERR, K_MDRADBEGR_EMF_MAX, K_PWG_IST_KICK_OFFSET, K_PWG_IST_MAFW_HYS_OFF, K_PWG_IST_MAFW_HYS_ON, K_PWG_IST_MAFW_MITTEL, K_V2NRAD_NKW, S_AUSW_V_MAFW, S_AUSW_V_MAFW_NL, S_FID_REKU_AKTIV, S_MAFW_USE_NEUE_EMF_LOGIK, S_MAFW_USE_NEUE_EMF_LOGIK2, S_NKW_ZKAFAK, S_PWG_IST_MAFW_FILTER, S_STATUS_USECASE, Md_rad_max_emf, Pwg_grad, Pwg_ist_mafw, St_mdipmfw, St_mdipmfw_B_bst, St_mdipmfw_B_emf_aktiv, St_mdipmfw_B_reku, St_mdipmfw_B_schlepp, Status_usecase_antr_lim, V_mafw)
}


def BMW_MOD_Mafw_Rueck(Dm_ab_fws_plaus: InMeasurement, Dmd_rad_spaf: InMeasurement, Md_rad_kriech: InMeasurement, Md_rad_max_zka: InMeasurement, Md_rad_min_zka: InMeasurement, Md_rad_steig: InMeasurement, Md_rad_wunsch_vb: InMeasurement, Neig_l_plaus: InMeasurement, Neig_l_qual_plaus: InMeasurement, Pwg_fvv_fakipm: InMeasurement, Pwg_ist: InMeasurement, Pwg_md_rad_null: InMeasurement, St_IfMgr_DrvrAsscSys: InMeasurement, St_IfMgr_DrvrAsscSys_B_pmalq_akt: InMeasurement, St_IfMgr_DrvrAsscSys_B_pmalq_anf: InMeasurement, St_fw: InMeasurement, St_fw_B_hschalt_komb: InMeasurement, St_fw_B_ldm_akt04: InMeasurement, St_fw_B_ldm_off: InMeasurement, St_fw_B_ldm_offhs: InMeasurement, St_fw_B_ldm_offreg: InMeasurement, St_fw_B_ldm_offrs: InMeasurement, St_fw_B_rschalt_komb: InMeasurement, St_kickdown: InMeasurement, St_kickdown_B_kickdown: InMeasurement, St_mdfw: InMeasurement, St_mdfw_B_sld_akt: InMeasurement, St_mdinfo_maw: InMeasurement, St_mdinfo_maw_B_fd_max_akt: InMeasurement, St_mdinfo_maw_B_fd_min_akt: InMeasurement, St_mdrdmk: InMeasurement, St_mdrdmk_B_fas_dcc: InMeasurement, St_mdrdmk_B_fas_sld: InMeasurement, V_mafw: InMeasurement, Var_hs: InMeasurement, K_DMD_RUNDUNGSFEHLER: BigDecimal, K_DPWG_NEG: BigDecimal, K_DPWG_POS: BigDecimal, K_KD_PWG_RUECK_ZKA: BigDecimal, K_MAX_PWG_RUECK_ZKA: BigDecimal, K_MD_AB_DIFF_KLEIN: BigDecimal, K_MD_RAD_RUECK_DIFF_MIN: BigDecimal, S_FIGA_PWG_RUECK_ON: String, S_KD_ON_PWG_RUECK: String, S_LDM_PWG_RUECK_ON: String, S_PWG_RUECK_USE_MAX: String, KF_MDRAD_STEIG: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MDR_ABV_D: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MDR_ABV_D_HS: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MDR_ABV_D_S: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MDR_ABV_ECO: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MDR_ABV_ECO2: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MDR_ABV_ECO2_HS: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MDR_ABV_ECO_HS: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MDR_ABV_ECO_S: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MDR_ABV_SPO: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MDR_ABV_SPO2: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MDR_ABV_SPO2_HS: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MDR_ABV_SPO_HS: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MDR_ABV_SPO_S: MapType[BigDecimal, BigDecimal, BigDecimal], KF_STEIG_AKT: MapType[BigDecimal, BigDecimal, BigDecimal], KF_STEIG_PWG: MapType[BigDecimal, BigDecimal, BigDecimal], KL_FWST_MAFW: CurveType[BigDecimal, BigDecimal], KL_STEIG_PWG_SCHUB: CurveType[BigDecimal, BigDecimal], KL_STEIG_RAMPE: CurveType[BigDecimal, BigDecimal], K_DM_AB_FWS_GRAD_MN: BigDecimal, K_DM_AB_FWS_GRAD_MX: BigDecimal, K_MDR_ABV_SPREIZ: BigDecimal, K_M_SW_VLIM: BigDecimal, K_NEIG_QUAL_APPLI: BigDecimal, NEIG_L_C: String, NEIG_L_V: BigDecimal, S_KOMP_STEIGSPA: BigDecimal, S_KRIECHMOMENT_ALS_MAX: BigDecimal, Md_rad_rueck: OutMeasurement, Pwg_virt_zka: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_Mafw_Rueck(a2lBin: A2LBinAdapter): Unit = {

val K_DMD_RUNDUNGSFEHLER: BigDecimal = a2lBin.readCharacteristicWithCast("K_DMD_RUNDUNGSFEHLER")
val K_DPWG_NEG: BigDecimal = a2lBin.readCharacteristicWithCast("K_DPWG_NEG")
val K_DPWG_POS: BigDecimal = a2lBin.readCharacteristicWithCast("K_DPWG_POS")
val K_KD_PWG_RUECK_ZKA: BigDecimal = a2lBin.readCharacteristicWithCast("K_KD_PWG_RUECK_ZKA")
val K_MAX_PWG_RUECK_ZKA: BigDecimal = a2lBin.readCharacteristicWithCast("K_MAX_PWG_RUECK_ZKA")
val K_MD_AB_DIFF_KLEIN: BigDecimal = a2lBin.readCharacteristicWithCast("K_MD_AB_DIFF_KLEIN")
val K_MD_RAD_RUECK_DIFF_MIN: BigDecimal = a2lBin.readCharacteristicWithCast("K_MD_RAD_RUECK_DIFF_MIN")
val S_FIGA_PWG_RUECK_ON: String = a2lBin.readCharacteristicWithCast("S_FIGA_PWG_RUECK_ON")
val S_KD_ON_PWG_RUECK: String = a2lBin.readCharacteristicWithCast("S_KD_ON_PWG_RUECK")
val S_LDM_PWG_RUECK_ON: String = a2lBin.readCharacteristicWithCast("S_LDM_PWG_RUECK_ON")
val S_PWG_RUECK_USE_MAX: String = a2lBin.readCharacteristicWithCast("S_PWG_RUECK_USE_MAX")
val KF_MDRAD_STEIG: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDRAD_STEIG")
val KF_MDR_ABV_D: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDR_ABV_D")
val KF_MDR_ABV_D_HS: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDR_ABV_D_HS")
val KF_MDR_ABV_D_S: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDR_ABV_D_S")
val KF_MDR_ABV_ECO: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDR_ABV_ECO")
val KF_MDR_ABV_ECO2: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDR_ABV_ECO2")
val KF_MDR_ABV_ECO2_HS: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDR_ABV_ECO2_HS")
val KF_MDR_ABV_ECO_HS: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDR_ABV_ECO_HS")
val KF_MDR_ABV_ECO_S: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDR_ABV_ECO_S")
val KF_MDR_ABV_SPO: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDR_ABV_SPO")
val KF_MDR_ABV_SPO2: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDR_ABV_SPO2")
val KF_MDR_ABV_SPO2_HS: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDR_ABV_SPO2_HS")
val KF_MDR_ABV_SPO_HS: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDR_ABV_SPO_HS")
val KF_MDR_ABV_SPO_S: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDR_ABV_SPO_S")
val KF_STEIG_AKT: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_STEIG_AKT")
val KF_STEIG_PWG: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_STEIG_PWG")
val KL_FWST_MAFW: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_FWST_MAFW")
val KL_STEIG_PWG_SCHUB: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_STEIG_PWG_SCHUB")
val KL_STEIG_RAMPE: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_STEIG_RAMPE")
val K_DM_AB_FWS_GRAD_MN: BigDecimal = a2lBin.readCharacteristicWithCast("K_DM_AB_FWS_GRAD_MN")
val K_DM_AB_FWS_GRAD_MX: BigDecimal = a2lBin.readCharacteristicWithCast("K_DM_AB_FWS_GRAD_MX")
val K_MDR_ABV_SPREIZ: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDR_ABV_SPREIZ")
val K_M_SW_VLIM: BigDecimal = a2lBin.readCharacteristicWithCast("K_M_SW_VLIM")
val K_NEIG_QUAL_APPLI: BigDecimal = a2lBin.readCharacteristicWithCast("K_NEIG_QUAL_APPLI")
val NEIG_L_C: String = a2lBin.readCharacteristicWithCast("NEIG_L_C")
val NEIG_L_V: BigDecimal = a2lBin.readCharacteristicWithCast("NEIG_L_V")
val S_KOMP_STEIGSPA: BigDecimal = a2lBin.readCharacteristicWithCast("S_KOMP_STEIGSPA")
val S_KRIECHMOMENT_ALS_MAX: BigDecimal = a2lBin.readCharacteristicWithCast("S_KRIECHMOMENT_ALS_MAX")
val Dm_ab_fws_plaus: InMeasurement = a2lBin.measurement("Dm_ab_fws_plaus")
val Dmd_rad_spaf: InMeasurement = a2lBin.measurement("Dmd_rad_spaf")
val Md_rad_kriech: InMeasurement = a2lBin.measurement("Md_rad_kriech")
val Md_rad_max_zka: InMeasurement = a2lBin.measurement("Md_rad_max_zka")
val Md_rad_min_zka: InMeasurement = a2lBin.measurement("Md_rad_min_zka")
val Md_rad_steig: InMeasurement = a2lBin.measurement("Md_rad_steig")
val Md_rad_wunsch_vb: InMeasurement = a2lBin.measurement("Md_rad_wunsch_vb")
val Neig_l_plaus: InMeasurement = a2lBin.measurement("Neig_l_plaus")
val Neig_l_qual_plaus: InMeasurement = a2lBin.measurement("Neig_l_qual_plaus")
val Pwg_fvv_fakipm: InMeasurement = a2lBin.measurement("Pwg_fvv_fakipm")
val Pwg_ist: InMeasurement = a2lBin.measurement("Pwg_ist")
val Pwg_md_rad_null: InMeasurement = a2lBin.measurement("Pwg_md_rad_null")
val St_IfMgr_DrvrAsscSys: InMeasurement = a2lBin.measurement("St_IfMgr_DrvrAsscSys")
val St_IfMgr_DrvrAsscSys_B_pmalq_akt: InMeasurement = a2lBin.measurement("St_IfMgr_DrvrAsscSys.B_pmalq_akt")
val St_IfMgr_DrvrAsscSys_B_pmalq_anf: InMeasurement = a2lBin.measurement("St_IfMgr_DrvrAsscSys.B_pmalq_anf")
val St_fw: InMeasurement = a2lBin.measurement("St_fw")
val St_fw_B_hschalt_komb: InMeasurement = a2lBin.measurement("St_fw.B_hschalt_komb")
val St_fw_B_ldm_akt04: InMeasurement = a2lBin.measurement("St_fw.B_ldm_akt04")
val St_fw_B_ldm_off: InMeasurement = a2lBin.measurement("St_fw.B_ldm_off")
val St_fw_B_ldm_offhs: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offhs")
val St_fw_B_ldm_offreg: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offreg")
val St_fw_B_ldm_offrs: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offrs")
val St_fw_B_rschalt_komb: InMeasurement = a2lBin.measurement("St_fw.B_rschalt_komb")
val St_kickdown: InMeasurement = a2lBin.measurement("St_kickdown")
val St_kickdown_B_kickdown: InMeasurement = a2lBin.measurement("St_kickdown.B_kickdown")
val St_mdfw: InMeasurement = a2lBin.measurement("St_mdfw")
val St_mdfw_B_sld_akt: InMeasurement = a2lBin.measurement("St_mdfw.B_sld_akt")
val St_mdinfo_maw: InMeasurement = a2lBin.measurement("St_mdinfo_maw")
val St_mdinfo_maw_B_fd_max_akt: InMeasurement = a2lBin.measurement("St_mdinfo_maw.B_fd_max_akt")
val St_mdinfo_maw_B_fd_min_akt: InMeasurement = a2lBin.measurement("St_mdinfo_maw.B_fd_min_akt")
val St_mdrdmk: InMeasurement = a2lBin.measurement("St_mdrdmk")
val St_mdrdmk_B_fas_dcc: InMeasurement = a2lBin.measurement("St_mdrdmk.B_fas_dcc")
val St_mdrdmk_B_fas_sld: InMeasurement = a2lBin.measurement("St_mdrdmk.B_fas_sld")
val V_mafw: InMeasurement = a2lBin.measurement("V_mafw")
val Var_hs: InMeasurement = a2lBin.measurement("Var_hs")
val Md_rad_rueck: OutMeasurement = a2lBin.measurement("Md_rad_rueck")
val Pwg_virt_zka: OutMeasurement = a2lBin.measurement("Pwg_virt_zka")

  BMW_MOD_Mafw_Rueck(Dm_ab_fws_plaus, Dmd_rad_spaf, Md_rad_kriech, Md_rad_max_zka, Md_rad_min_zka, Md_rad_steig, Md_rad_wunsch_vb, Neig_l_plaus, Neig_l_qual_plaus, Pwg_fvv_fakipm, Pwg_ist, Pwg_md_rad_null, St_IfMgr_DrvrAsscSys, St_IfMgr_DrvrAsscSys_B_pmalq_akt, St_IfMgr_DrvrAsscSys_B_pmalq_anf, St_fw, St_fw_B_hschalt_komb, St_fw_B_ldm_akt04, St_fw_B_ldm_off, St_fw_B_ldm_offhs, St_fw_B_ldm_offreg, St_fw_B_ldm_offrs, St_fw_B_rschalt_komb, St_kickdown, St_kickdown_B_kickdown, St_mdfw, St_mdfw_B_sld_akt, St_mdinfo_maw, St_mdinfo_maw_B_fd_max_akt, St_mdinfo_maw_B_fd_min_akt, St_mdrdmk, St_mdrdmk_B_fas_dcc, St_mdrdmk_B_fas_sld, V_mafw, Var_hs, K_DMD_RUNDUNGSFEHLER, K_DPWG_NEG, K_DPWG_POS, K_KD_PWG_RUECK_ZKA, K_MAX_PWG_RUECK_ZKA, K_MD_AB_DIFF_KLEIN, K_MD_RAD_RUECK_DIFF_MIN, S_FIGA_PWG_RUECK_ON, S_KD_ON_PWG_RUECK, S_LDM_PWG_RUECK_ON, S_PWG_RUECK_USE_MAX, KF_MDRAD_STEIG, KF_MDR_ABV_D, KF_MDR_ABV_D_HS, KF_MDR_ABV_D_S, KF_MDR_ABV_ECO, KF_MDR_ABV_ECO2, KF_MDR_ABV_ECO2_HS, KF_MDR_ABV_ECO_HS, KF_MDR_ABV_ECO_S, KF_MDR_ABV_SPO, KF_MDR_ABV_SPO2, KF_MDR_ABV_SPO2_HS, KF_MDR_ABV_SPO_HS, KF_MDR_ABV_SPO_S, KF_STEIG_AKT, KF_STEIG_PWG, KL_FWST_MAFW, KL_STEIG_PWG_SCHUB, KL_STEIG_RAMPE, K_DM_AB_FWS_GRAD_MN, K_DM_AB_FWS_GRAD_MX, K_MDR_ABV_SPREIZ, K_M_SW_VLIM, K_NEIG_QUAL_APPLI, NEIG_L_C, NEIG_L_V, S_KOMP_STEIGSPA, S_KRIECHMOMENT_ALS_MAX, Md_rad_rueck, Pwg_virt_zka)
}


def BMW_MOD_Mafw_Schlepp(BMWtqw_tqw_EngDfcoMinStat_sw: InMeasurement, St_sai_hyb_bits: InMeasurement, St_sai_hyb_bits_B_segelpedal_hyb: InMeasurement, Status_usecase_antr_lim: InMeasurement, V_mafw: InMeasurement, Zka_sfak: InMeasurement, KF_MD_RAD_SCHLEPP_MAX: MapType[BigDecimal, BigDecimal, BigDecimal], KL_MD_RAD_SCHLEPP_VM_BEGR: CurveType[BigDecimal, BigDecimal], KL_MD_RAD_SCHLEPP_VM_SSTAB: CurveType[BigDecimal, BigDecimal], KL_UC_SCHLEPP: CurveType[BigDecimal, BigDecimal], S_MD_RAD_SCHLEPP: String, Md_rad_schlepp_max: OutMeasurement, Md_rad_schlepp_soll_roh: OutMeasurement, St_uc_schlepp: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_Mafw_Schlepp(a2lBin: A2LBinAdapter): Unit = {

val KF_MD_RAD_SCHLEPP_MAX: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MD_RAD_SCHLEPP_MAX")
val KL_MD_RAD_SCHLEPP_VM_BEGR: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MD_RAD_SCHLEPP_VM_BEGR")
val KL_MD_RAD_SCHLEPP_VM_SSTAB: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MD_RAD_SCHLEPP_VM_SSTAB")
val KL_UC_SCHLEPP: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_UC_SCHLEPP")
val S_MD_RAD_SCHLEPP: String = a2lBin.readCharacteristicWithCast("S_MD_RAD_SCHLEPP")
val BMWtqw_tqw_EngDfcoMinStat_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_EngDfcoMinStat_sw")
val St_sai_hyb_bits: InMeasurement = a2lBin.measurement("St_sai_hyb_bits")
val St_sai_hyb_bits_B_segelpedal_hyb: InMeasurement = a2lBin.measurement("St_sai_hyb_bits.B_segelpedal_hyb")
val Status_usecase_antr_lim: InMeasurement = a2lBin.measurement("Status_usecase_antr_lim")
val V_mafw: InMeasurement = a2lBin.measurement("V_mafw")
val Zka_sfak: InMeasurement = a2lBin.measurement("Zka_sfak")
val Md_rad_schlepp_max: OutMeasurement = a2lBin.measurement("Md_rad_schlepp_max")
val Md_rad_schlepp_soll_roh: OutMeasurement = a2lBin.measurement("Md_rad_schlepp_soll_roh")
val St_uc_schlepp: OutMeasurement = a2lBin.measurement("St_uc_schlepp")

  BMW_MOD_Mafw_Schlepp(BMWtqw_tqw_EngDfcoMinStat_sw, St_sai_hyb_bits, St_sai_hyb_bits_B_segelpedal_hyb, Status_usecase_antr_lim, V_mafw, Zka_sfak, KF_MD_RAD_SCHLEPP_MAX, KL_MD_RAD_SCHLEPP_VM_BEGR, KL_MD_RAD_SCHLEPP_VM_SSTAB, KL_UC_SCHLEPP, S_MD_RAD_SCHLEPP, Md_rad_schlepp_max, Md_rad_schlepp_soll_roh, St_uc_schlepp)
}


def BMW_MOD_Mafw_SteigKomp(Dm_ab_fws_plaus: InMeasurement, Dmd_rad_spaf: InMeasurement, Md_rad_schlepp_soll_roh: InMeasurement, Neig_l_plaus: InMeasurement, Neig_l_qual_plaus: InMeasurement, Pwg_fvv_fakipm: InMeasurement, Pwg_md_diff: InMeasurement, V_mafw: InMeasurement, KF_MDRAD_STEIG: MapType[BigDecimal, BigDecimal, BigDecimal], KF_STEIG_AKT: MapType[BigDecimal, BigDecimal, BigDecimal], KF_STEIG_PWG: MapType[BigDecimal, BigDecimal, BigDecimal], KL_FWST_MAFW: CurveType[BigDecimal, BigDecimal], KL_STEIG_PWG_SCHUB: CurveType[BigDecimal, BigDecimal], KL_STEIG_RAMPE: CurveType[BigDecimal, BigDecimal], K_DM_AB_FWS_GRAD_MN: BigDecimal, K_DM_AB_FWS_GRAD_MX: BigDecimal, K_NEIG_QUAL_APPLI: BigDecimal, K_OFFSET_BERGAB: BigDecimal, NEIG_L_C: String, NEIG_L_V: BigDecimal, S_CP434796: String, S_KOMP_STEIGSPA: BigDecimal, K_M_SW_VLIM: BigDecimal, Dm_ab_fws_begr: OutMeasurement, Md_rad_steig: OutMeasurement, Md_rad_steig_schub: OutMeasurement, Md_rad_steig_zug: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_Mafw_SteigKomp(a2lBin: A2LBinAdapter): Unit = {

val KF_MDRAD_STEIG: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MDRAD_STEIG")
val KF_STEIG_AKT: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_STEIG_AKT")
val KF_STEIG_PWG: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_STEIG_PWG")
val KL_FWST_MAFW: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_FWST_MAFW")
val KL_STEIG_PWG_SCHUB: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_STEIG_PWG_SCHUB")
val KL_STEIG_RAMPE: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_STEIG_RAMPE")
val K_DM_AB_FWS_GRAD_MN: BigDecimal = a2lBin.readCharacteristicWithCast("K_DM_AB_FWS_GRAD_MN")
val K_DM_AB_FWS_GRAD_MX: BigDecimal = a2lBin.readCharacteristicWithCast("K_DM_AB_FWS_GRAD_MX")
val K_NEIG_QUAL_APPLI: BigDecimal = a2lBin.readCharacteristicWithCast("K_NEIG_QUAL_APPLI")
val K_OFFSET_BERGAB: BigDecimal = a2lBin.readCharacteristicWithCast("K_OFFSET_BERGAB")
val NEIG_L_C: String = a2lBin.readCharacteristicWithCast("NEIG_L_C")
val NEIG_L_V: BigDecimal = a2lBin.readCharacteristicWithCast("NEIG_L_V")
val S_CP434796: String = a2lBin.readCharacteristicWithCast("S_CP434796")
val S_KOMP_STEIGSPA: BigDecimal = a2lBin.readCharacteristicWithCast("S_KOMP_STEIGSPA")
val K_M_SW_VLIM: BigDecimal = a2lBin.readCharacteristicWithCast("K_M_SW_VLIM")
val Dm_ab_fws_plaus: InMeasurement = a2lBin.measurement("Dm_ab_fws_plaus")
val Dmd_rad_spaf: InMeasurement = a2lBin.measurement("Dmd_rad_spaf")
val Md_rad_schlepp_soll_roh: InMeasurement = a2lBin.measurement("Md_rad_schlepp_soll_roh")
val Neig_l_plaus: InMeasurement = a2lBin.measurement("Neig_l_plaus")
val Neig_l_qual_plaus: InMeasurement = a2lBin.measurement("Neig_l_qual_plaus")
val Pwg_fvv_fakipm: InMeasurement = a2lBin.measurement("Pwg_fvv_fakipm")
val Pwg_md_diff: InMeasurement = a2lBin.measurement("Pwg_md_diff")
val V_mafw: InMeasurement = a2lBin.measurement("V_mafw")
val Dm_ab_fws_begr: OutMeasurement = a2lBin.measurement("Dm_ab_fws_begr")
val Md_rad_steig: OutMeasurement = a2lBin.measurement("Md_rad_steig")
val Md_rad_steig_schub: OutMeasurement = a2lBin.measurement("Md_rad_steig_schub")
val Md_rad_steig_zug: OutMeasurement = a2lBin.measurement("Md_rad_steig_zug")

  BMW_MOD_Mafw_SteigKomp(Dm_ab_fws_plaus, Dmd_rad_spaf, Md_rad_schlepp_soll_roh, Neig_l_plaus, Neig_l_qual_plaus, Pwg_fvv_fakipm, Pwg_md_diff, V_mafw, KF_MDRAD_STEIG, KF_STEIG_AKT, KF_STEIG_PWG, KL_FWST_MAFW, KL_STEIG_PWG_SCHUB, KL_STEIG_RAMPE, K_DM_AB_FWS_GRAD_MN, K_DM_AB_FWS_GRAD_MX, K_NEIG_QUAL_APPLI, K_OFFSET_BERGAB, NEIG_L_C, NEIG_L_V, S_CP434796, S_KOMP_STEIGSPA, K_M_SW_VLIM, Dm_ab_fws_begr, Md_rad_steig, Md_rad_steig_schub, Md_rad_steig_zug)
}


def BMW_MOD_Mafw_Wunsch(BMWtqw_tqw_DtHaAvlStatIsc_sw: InMeasurement, Md_rad_kriech: InMeasurement, Md_rad_max_rk: InMeasurement, Md_rad_max_zka: InMeasurement, Md_rad_min_zka: InMeasurement, Md_rad_schlepp_soll: InMeasurement, Md_rad_steig_zug: InMeasurement, Mdr_pedal_neg: InMeasurement, Mdr_pedal_pos: InMeasurement, Pwg_ist_mafw: InMeasurement, Pwg_md_rad_null: InMeasurement, S_KRIECHMOMENT_ALS_MAX: BigDecimal, CW_NEDI_LLRADD_CAN: Array[String], Md_rad_pedal: OutMeasurement, Md_rad_pedal_ap: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_Mafw_Wunsch(a2lBin: A2LBinAdapter): Unit = {

val S_KRIECHMOMENT_ALS_MAX: BigDecimal = a2lBin.readCharacteristicWithCast("S_KRIECHMOMENT_ALS_MAX")
val CW_NEDI_LLRADD_CAN: Array[String] = a2lBin.readCharacteristicWithCast("CW_NEDI_LLRADD_CAN")
val BMWtqw_tqw_DtHaAvlStatIsc_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_DtHaAvlStatIsc_sw")
val Md_rad_kriech: InMeasurement = a2lBin.measurement("Md_rad_kriech")
val Md_rad_max_rk: InMeasurement = a2lBin.measurement("Md_rad_max_rk")
val Md_rad_max_zka: InMeasurement = a2lBin.measurement("Md_rad_max_zka")
val Md_rad_min_zka: InMeasurement = a2lBin.measurement("Md_rad_min_zka")
val Md_rad_schlepp_soll: InMeasurement = a2lBin.measurement("Md_rad_schlepp_soll")
val Md_rad_steig_zug: InMeasurement = a2lBin.measurement("Md_rad_steig_zug")
val Mdr_pedal_neg: InMeasurement = a2lBin.measurement("Mdr_pedal_neg")
val Mdr_pedal_pos: InMeasurement = a2lBin.measurement("Mdr_pedal_pos")
val Pwg_ist_mafw: InMeasurement = a2lBin.measurement("Pwg_ist_mafw")
val Pwg_md_rad_null: InMeasurement = a2lBin.measurement("Pwg_md_rad_null")
val Md_rad_pedal: OutMeasurement = a2lBin.measurement("Md_rad_pedal")
val Md_rad_pedal_ap: OutMeasurement = a2lBin.measurement("Md_rad_pedal_ap")

  BMW_MOD_Mafw_Wunsch(BMWtqw_tqw_DtHaAvlStatIsc_sw, Md_rad_kriech, Md_rad_max_rk, Md_rad_max_zka, Md_rad_min_zka, Md_rad_schlepp_soll, Md_rad_steig_zug, Mdr_pedal_neg, Mdr_pedal_pos, Pwg_ist_mafw, Pwg_md_rad_null, S_KRIECHMOMENT_ALS_MAX, CW_NEDI_LLRADD_CAN, Md_rad_pedal, Md_rad_pedal_ap)
}


def BMW_MOD_Mafw_ZkaFak(BMWbdy_b_RvsGear_bo: InMeasurement, I_eff: InMeasurement, I_eff_f: InMeasurement, I_ges_stat: InMeasurement, I_ha: InMeasurement, N_ab: InMeasurement, N_em1_ref: InMeasurement, Nkw_ref: InMeasurement, St_dsc_can: InMeasurement, St_fahrzust_fzg: InMeasurement, St_getrdaten: InMeasurement, St_getrdaten_B_gangwechsel_gs: InMeasurement, Status_usecase_antr_lim: InMeasurement, Var_hs: InMeasurement, KF_ZKA_SPRUNG: MapType[BigDecimal, BigDecimal, BigDecimal], KF_ZKA_SPRUNG_1: MapType[BigDecimal, BigDecimal, BigDecimal], KF_ZKA_SPRUNG_1_AT: MapType[BigDecimal, BigDecimal, BigDecimal], KF_ZKA_SPRUNG_AT: MapType[BigDecimal, BigDecimal, BigDecimal], KF_ZKA_SSPRUNG: MapType[BigDecimal, BigDecimal, BigDecimal], KF_ZKA_SSPRUNG_1: MapType[BigDecimal, BigDecimal, BigDecimal], KF_ZKA_SSPRUNG_1_AT: MapType[BigDecimal, BigDecimal, BigDecimal], KF_ZKA_SSPRUNG_AT: MapType[BigDecimal, BigDecimal, BigDecimal], KL_UC_ZKASPRUNG_SCHUB: CurveType[BigDecimal, BigDecimal], KL_UC_ZKASPRUNG_ZUG: CurveType[BigDecimal, BigDecimal], KL_ZKAFAK_SCHALT_ON: CurveType[BigDecimal, BigDecimal], K_F_REGEN_DSC: BigDecimal, K_F_REGEN_MAX: BigDecimal, K_I_EFF_REGEN_RUECK: BigDecimal, K_ST_DSC_REGEN: BigDecimal, K_ST_FAHRZUST_REGEN: BigDecimal, K_ZKAFAK_SCHALT_OFF: BigDecimal, K_ZKASPR_OFF: BigDecimal, K_ZKASPR_ON: BigDecimal, K_ZKASSPR_OFF: BigDecimal, K_ZKASSPR_ON: BigDecimal, K_ZKA_KORR_MAX: BigDecimal, K_ZKA_KORR_MIN: BigDecimal, K_ZKA_SKORR_MAX: BigDecimal, K_ZKA_SKORR_MIN: BigDecimal, S_MAFW_I_EFF: String, S_NKW_REF: String, Nkw_zkafak: OutMeasurement, Zka_fak: OutMeasurement, Zka_fak_fak: OutMeasurement, Zka_sfak: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_Mafw_ZkaFak(a2lBin: A2LBinAdapter): Unit = {

val KF_ZKA_SPRUNG: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_ZKA_SPRUNG")
val KF_ZKA_SPRUNG_1: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_ZKA_SPRUNG_1")
val KF_ZKA_SPRUNG_1_AT: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_ZKA_SPRUNG_1_AT")
val KF_ZKA_SPRUNG_AT: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_ZKA_SPRUNG_AT")
val KF_ZKA_SSPRUNG: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_ZKA_SSPRUNG")
val KF_ZKA_SSPRUNG_1: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_ZKA_SSPRUNG_1")
val KF_ZKA_SSPRUNG_1_AT: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_ZKA_SSPRUNG_1_AT")
val KF_ZKA_SSPRUNG_AT: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_ZKA_SSPRUNG_AT")
val KL_UC_ZKASPRUNG_SCHUB: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_UC_ZKASPRUNG_SCHUB")
val KL_UC_ZKASPRUNG_ZUG: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_UC_ZKASPRUNG_ZUG")
val KL_ZKAFAK_SCHALT_ON: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_ZKAFAK_SCHALT_ON")
val K_F_REGEN_DSC: BigDecimal = a2lBin.readCharacteristicWithCast("K_F_REGEN_DSC")
val K_F_REGEN_MAX: BigDecimal = a2lBin.readCharacteristicWithCast("K_F_REGEN_MAX")
val K_I_EFF_REGEN_RUECK: BigDecimal = a2lBin.readCharacteristicWithCast("K_I_EFF_REGEN_RUECK")
val K_ST_DSC_REGEN: BigDecimal = a2lBin.readCharacteristicWithCast("K_ST_DSC_REGEN")
val K_ST_FAHRZUST_REGEN: BigDecimal = a2lBin.readCharacteristicWithCast("K_ST_FAHRZUST_REGEN")
val K_ZKAFAK_SCHALT_OFF: BigDecimal = a2lBin.readCharacteristicWithCast("K_ZKAFAK_SCHALT_OFF")
val K_ZKASPR_OFF: BigDecimal = a2lBin.readCharacteristicWithCast("K_ZKASPR_OFF")
val K_ZKASPR_ON: BigDecimal = a2lBin.readCharacteristicWithCast("K_ZKASPR_ON")
val K_ZKASSPR_OFF: BigDecimal = a2lBin.readCharacteristicWithCast("K_ZKASSPR_OFF")
val K_ZKASSPR_ON: BigDecimal = a2lBin.readCharacteristicWithCast("K_ZKASSPR_ON")
val K_ZKA_KORR_MAX: BigDecimal = a2lBin.readCharacteristicWithCast("K_ZKA_KORR_MAX")
val K_ZKA_KORR_MIN: BigDecimal = a2lBin.readCharacteristicWithCast("K_ZKA_KORR_MIN")
val K_ZKA_SKORR_MAX: BigDecimal = a2lBin.readCharacteristicWithCast("K_ZKA_SKORR_MAX")
val K_ZKA_SKORR_MIN: BigDecimal = a2lBin.readCharacteristicWithCast("K_ZKA_SKORR_MIN")
val S_MAFW_I_EFF: String = a2lBin.readCharacteristicWithCast("S_MAFW_I_EFF")
val S_NKW_REF: String = a2lBin.readCharacteristicWithCast("S_NKW_REF")
val BMWbdy_b_RvsGear_bo: InMeasurement = a2lBin.measurement("BMWbdy_b_RvsGear_bo")
val I_eff: InMeasurement = a2lBin.measurement("I_eff")
val I_eff_f: InMeasurement = a2lBin.measurement("I_eff_f")
val I_ges_stat: InMeasurement = a2lBin.measurement("I_ges_stat")
val I_ha: InMeasurement = a2lBin.measurement("I_ha")
val N_ab: InMeasurement = a2lBin.measurement("N_ab")
val N_em1_ref: InMeasurement = a2lBin.measurement("N_em1_ref")
val Nkw_ref: InMeasurement = a2lBin.measurement("Nkw_ref")
val St_dsc_can: InMeasurement = a2lBin.measurement("St_dsc_can")
val St_fahrzust_fzg: InMeasurement = a2lBin.measurement("St_fahrzust_fzg")
val St_getrdaten: InMeasurement = a2lBin.measurement("St_getrdaten")
val St_getrdaten_B_gangwechsel_gs: InMeasurement = a2lBin.measurement("St_getrdaten.B_gangwechsel_gs")
val Status_usecase_antr_lim: InMeasurement = a2lBin.measurement("Status_usecase_antr_lim")
val Var_hs: InMeasurement = a2lBin.measurement("Var_hs")
val Nkw_zkafak: OutMeasurement = a2lBin.measurement("Nkw_zkafak")
val Zka_fak: OutMeasurement = a2lBin.measurement("Zka_fak")
val Zka_fak_fak: OutMeasurement = a2lBin.measurement("Zka_fak_fak")
val Zka_sfak: OutMeasurement = a2lBin.measurement("Zka_sfak")

  BMW_MOD_Mafw_ZkaFak(BMWbdy_b_RvsGear_bo, I_eff, I_eff_f, I_ges_stat, I_ha, N_ab, N_em1_ref, Nkw_ref, St_dsc_can, St_fahrzust_fzg, St_getrdaten, St_getrdaten_B_gangwechsel_gs, Status_usecase_antr_lim, Var_hs, KF_ZKA_SPRUNG, KF_ZKA_SPRUNG_1, KF_ZKA_SPRUNG_1_AT, KF_ZKA_SPRUNG_AT, KF_ZKA_SSPRUNG, KF_ZKA_SSPRUNG_1, KF_ZKA_SSPRUNG_1_AT, KF_ZKA_SSPRUNG_AT, KL_UC_ZKASPRUNG_SCHUB, KL_UC_ZKASPRUNG_ZUG, KL_ZKAFAK_SCHALT_ON, K_F_REGEN_DSC, K_F_REGEN_MAX, K_I_EFF_REGEN_RUECK, K_ST_DSC_REGEN, K_ST_FAHRZUST_REGEN, K_ZKAFAK_SCHALT_OFF, K_ZKASPR_OFF, K_ZKASPR_ON, K_ZKASSPR_OFF, K_ZKASSPR_ON, K_ZKA_KORR_MAX, K_ZKA_KORR_MIN, K_ZKA_SKORR_MAX, K_ZKA_SKORR_MIN, S_MAFW_I_EFF, S_NKW_REF, Nkw_zkafak, Zka_fak, Zka_fak_fak, Zka_sfak)
}


def BMW_MOD_Mxk_AxcSpAccFilt(BMWas_tq_Em1StatMax_sw: InMeasurement, BMWtqc_Rat_GbxWhlHaxl: InMeasurement, BMWtqw_tqw_AxcEaSpFildPre_sw: InMeasurement, BMWtqw_tqw_AxcHaLdcSpFildPEfR_sw: InMeasurement, BMWtqw_tqw_AxcHaLdcSpFildPre_sw: InMeasurement, BMWtqw_tqw_AxcHaLpaSpFildPre_sw: InMeasurement, BMWtqw_tqw_AxcHaSpFildPre_sw: InMeasurement, B_figa_ls_mawf: InMeasurement, Dmdp_mawf: InMeasurement, Dmls_mawf: InMeasurement, Md_rad_fzdyn: InMeasurement, N_vm_soll_egs_plaus: InMeasurement, St_antrieb_wunsch: InMeasurement, St_as_bzm_vm: InMeasurement, St_as_bzm_vm_B_vm_verfuegbar: InMeasurement, St_as_bzm_vm_B_vm_wunsch: InMeasurement, St_as_bzm_vm_B_vmstplock: InMeasurement, St_mdar0: InMeasurement, St_mdar0_B_edp: InMeasurement, St_mdar0_B_elsd: InMeasurement, St_mdar0_B_lsd: InMeasurement, St_mdar1: InMeasurement, St_mdar1_B_dash: InMeasurement, St_mdar1_B_kf_at: InMeasurement, Status_antrieb_ist: InMeasurement, Status_startstopp: InMeasurement, Status_startvar: InMeasurement, V_fzg_plaus: InMeasurement, KF_ACCFILT_RESPONSE: MapType[BigDecimal, BigDecimal, BigDecimal], KF_DM_RAD_LIM_EM_MAX_STAT: MapType[BigDecimal, BigDecimal, BigDecimal], KL_F_DM_RAD_LIM_STARTVAR: CurveType[BigDecimal, BigDecimal], K_F_FILT_LIM_CONST_ACC: BigDecimal, K_MD_MIN_FZDYN_ZUG: BigDecimal, K_MD_RAD_DELTA_MAX_ACCFILTOFF: BigDecimal, K_POS_UCANTR_KRITERIUM_ACCFILT: BigDecimal, S_CP366810_LPA: String, S_USE_ACCFILT_VMKRITERIUM: String, BMWtqw_swi_CfgDt2VehFa_C: BigDecimal, BMWtqw_swi_CfgDt2VehRa_C: BigDecimal, BMWtqw_tqw_AxcHaLdcSpFildEfR_sw: OutMeasurement, BMWtqw_tqw_AxcHaLdcSpFild_sw: OutMeasurement, BMWtqw_tqw_AxcHaLpaSpFild_sw: OutMeasurement, BMWtqw_tqw_AxcHaSpFild_sw: OutMeasurement, Dmradsoll_hybachs_mxk: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_Mxk_AxcSpAccFilt(a2lBin: A2LBinAdapter): Unit = {

val KF_ACCFILT_RESPONSE: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_ACCFILT_RESPONSE")
val KF_DM_RAD_LIM_EM_MAX_STAT: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_DM_RAD_LIM_EM_MAX_STAT")
val KL_F_DM_RAD_LIM_STARTVAR: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_F_DM_RAD_LIM_STARTVAR")
val K_F_FILT_LIM_CONST_ACC: BigDecimal = a2lBin.readCharacteristicWithCast("K_F_FILT_LIM_CONST_ACC")
val K_MD_MIN_FZDYN_ZUG: BigDecimal = a2lBin.readCharacteristicWithCast("K_MD_MIN_FZDYN_ZUG")
val K_MD_RAD_DELTA_MAX_ACCFILTOFF: BigDecimal = a2lBin.readCharacteristicWithCast("K_MD_RAD_DELTA_MAX_ACCFILTOFF")
val K_POS_UCANTR_KRITERIUM_ACCFILT: BigDecimal = a2lBin.readCharacteristicWithCast("K_POS_UCANTR_KRITERIUM_ACCFILT")
val S_CP366810_LPA: String = a2lBin.readCharacteristicWithCast("S_CP366810_LPA")
val S_USE_ACCFILT_VMKRITERIUM: String = a2lBin.readCharacteristicWithCast("S_USE_ACCFILT_VMKRITERIUM")
val BMWtqw_swi_CfgDt2VehFa_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_swi_CfgDt2VehFa_C")
val BMWtqw_swi_CfgDt2VehRa_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_swi_CfgDt2VehRa_C")
val BMWas_tq_Em1StatMax_sw: InMeasurement = a2lBin.measurement("BMWas_tq_Em1StatMax_sw")
val BMWtqc_Rat_GbxWhlHaxl: InMeasurement = a2lBin.measurement("BMWtqc_Rat_GbxWhlHaxl")
val BMWtqw_tqw_AxcEaSpFildPre_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcEaSpFildPre_sw")
val BMWtqw_tqw_AxcHaLdcSpFildPEfR_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcHaLdcSpFildPEfR_sw")
val BMWtqw_tqw_AxcHaLdcSpFildPre_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcHaLdcSpFildPre_sw")
val BMWtqw_tqw_AxcHaLpaSpFildPre_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcHaLpaSpFildPre_sw")
val BMWtqw_tqw_AxcHaSpFildPre_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcHaSpFildPre_sw")
val B_figa_ls_mawf: InMeasurement = a2lBin.measurement("B_figa_ls_mawf")
val Dmdp_mawf: InMeasurement = a2lBin.measurement("Dmdp_mawf")
val Dmls_mawf: InMeasurement = a2lBin.measurement("Dmls_mawf")
val Md_rad_fzdyn: InMeasurement = a2lBin.measurement("Md_rad_fzdyn")
val N_vm_soll_egs_plaus: InMeasurement = a2lBin.measurement("N_vm_soll_egs_plaus")
val St_antrieb_wunsch: InMeasurement = a2lBin.measurement("St_antrieb_wunsch")
val St_as_bzm_vm: InMeasurement = a2lBin.measurement("St_as_bzm_vm")
val St_as_bzm_vm_B_vm_verfuegbar: InMeasurement = a2lBin.measurement("St_as_bzm_vm.B_vm_verfuegbar")
val St_as_bzm_vm_B_vm_wunsch: InMeasurement = a2lBin.measurement("St_as_bzm_vm.B_vm_wunsch")
val St_as_bzm_vm_B_vmstplock: InMeasurement = a2lBin.measurement("St_as_bzm_vm.B_vmstplock")
val St_mdar0: InMeasurement = a2lBin.measurement("St_mdar0")
val St_mdar0_B_edp: InMeasurement = a2lBin.measurement("St_mdar0.B_edp")
val St_mdar0_B_elsd: InMeasurement = a2lBin.measurement("St_mdar0.B_elsd")
val St_mdar0_B_lsd: InMeasurement = a2lBin.measurement("St_mdar0.B_lsd")
val St_mdar1: InMeasurement = a2lBin.measurement("St_mdar1")
val St_mdar1_B_dash: InMeasurement = a2lBin.measurement("St_mdar1.B_dash")
val St_mdar1_B_kf_at: InMeasurement = a2lBin.measurement("St_mdar1.B_kf_at")
val Status_antrieb_ist: InMeasurement = a2lBin.measurement("Status_antrieb_ist")
val Status_startstopp: InMeasurement = a2lBin.measurement("Status_startstopp")
val Status_startvar: InMeasurement = a2lBin.measurement("Status_startvar")
val V_fzg_plaus: InMeasurement = a2lBin.measurement("V_fzg_plaus")
val BMWtqw_tqw_AxcHaLdcSpFildEfR_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcHaLdcSpFildEfR_sw")
val BMWtqw_tqw_AxcHaLdcSpFild_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcHaLdcSpFild_sw")
val BMWtqw_tqw_AxcHaLpaSpFild_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcHaLpaSpFild_sw")
val BMWtqw_tqw_AxcHaSpFild_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcHaSpFild_sw")
val Dmradsoll_hybachs_mxk: OutMeasurement = a2lBin.measurement("Dmradsoll_hybachs_mxk")

  BMW_MOD_Mxk_AxcSpAccFilt(BMWas_tq_Em1StatMax_sw, BMWtqc_Rat_GbxWhlHaxl, BMWtqw_tqw_AxcEaSpFildPre_sw, BMWtqw_tqw_AxcHaLdcSpFildPEfR_sw, BMWtqw_tqw_AxcHaLdcSpFildPre_sw, BMWtqw_tqw_AxcHaLpaSpFildPre_sw, BMWtqw_tqw_AxcHaSpFildPre_sw, B_figa_ls_mawf, Dmdp_mawf, Dmls_mawf, Md_rad_fzdyn, N_vm_soll_egs_plaus, St_antrieb_wunsch, St_as_bzm_vm, St_as_bzm_vm_B_vm_verfuegbar, St_as_bzm_vm_B_vm_wunsch, St_as_bzm_vm_B_vmstplock, St_mdar0, St_mdar0_B_edp, St_mdar0_B_elsd, St_mdar0_B_lsd, St_mdar1, St_mdar1_B_dash, St_mdar1_B_kf_at, Status_antrieb_ist, Status_startstopp, Status_startvar, V_fzg_plaus, KF_ACCFILT_RESPONSE, KF_DM_RAD_LIM_EM_MAX_STAT, KL_F_DM_RAD_LIM_STARTVAR, K_F_FILT_LIM_CONST_ACC, K_MD_MIN_FZDYN_ZUG, K_MD_RAD_DELTA_MAX_ACCFILTOFF, K_POS_UCANTR_KRITERIUM_ACCFILT, S_CP366810_LPA, S_USE_ACCFILT_VMKRITERIUM, BMWtqw_swi_CfgDt2VehFa_C, BMWtqw_swi_CfgDt2VehRa_C, BMWtqw_tqw_AxcHaLdcSpFildEfR_sw, BMWtqw_tqw_AxcHaLdcSpFild_sw, BMWtqw_tqw_AxcHaLpaSpFild_sw, BMWtqw_tqw_AxcHaSpFild_sw, Dmradsoll_hybachs_mxk)
}


def BMW_MOD_Mxk_AxcSpBasc(BMWtqw_tqw_AxcDtHaMaxDyn_sw: InMeasurement, BMWtqw_tqw_AxcDtHaMinDyn_sw: InMeasurement, Md_rad_fzdyn_int: InMeasurement, MD_RAD_FZDYN_HA_C: String, MD_RAD_FZDYN_HA_V: BigDecimal, BMWtqw_swi_CfgDt2VehRa_C: BigDecimal): Unit = {
 ???
}

def BMW_MOD_Mxk_AxcSpBasc(a2lBin: A2LBinAdapter): Unit = {

val MD_RAD_FZDYN_HA_C: String = a2lBin.readCharacteristicWithCast("MD_RAD_FZDYN_HA_C")
val MD_RAD_FZDYN_HA_V: BigDecimal = a2lBin.readCharacteristicWithCast("MD_RAD_FZDYN_HA_V")
val BMWtqw_swi_CfgDt2VehRa_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_swi_CfgDt2VehRa_C")
val BMWtqw_tqw_AxcDtHaMaxDyn_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcDtHaMaxDyn_sw")
val BMWtqw_tqw_AxcDtHaMinDyn_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcDtHaMinDyn_sw")
val Md_rad_fzdyn_int: InMeasurement = a2lBin.measurement("Md_rad_fzdyn_int")


  BMW_MOD_Mxk_AxcSpBasc(BMWtqw_tqw_AxcDtHaMaxDyn_sw, BMWtqw_tqw_AxcDtHaMinDyn_sw, Md_rad_fzdyn_int, MD_RAD_FZDYN_HA_C, MD_RAD_FZDYN_HA_V, BMWtqw_swi_CfgDt2VehRa_C)
}


def BMW_MOD_Mxk_AxcSpFild(BMWtqw_tqw_AxcDtHaMaxDyn_sw: InMeasurement, BMWtqw_tqw_AxcDtHaMinDyn_sw: InMeasurement, Md_rad_fzdyn_int: InMeasurement, BMWtqw_tqw_AxcEaSpFildPre_sw: OutMeasurement, BMWtqw_tqw_AxcHaLpaSpFildPre_sw: OutMeasurement, BMWtqw_tqw_AxcHaSpFildPre_sw: OutMeasurement, B_axc_ha_start_rmp_akt: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_Mxk_AxcSpFild(a2lBin: A2LBinAdapter): Unit = {


val BMWtqw_tqw_AxcDtHaMaxDyn_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcDtHaMaxDyn_sw")
val BMWtqw_tqw_AxcDtHaMinDyn_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcDtHaMinDyn_sw")
val Md_rad_fzdyn_int: InMeasurement = a2lBin.measurement("Md_rad_fzdyn_int")
val BMWtqw_tqw_AxcEaSpFildPre_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcEaSpFildPre_sw")
val BMWtqw_tqw_AxcHaLpaSpFildPre_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcHaLpaSpFildPre_sw")
val BMWtqw_tqw_AxcHaSpFildPre_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcHaSpFildPre_sw")
val B_axc_ha_start_rmp_akt: OutMeasurement = a2lBin.measurement("B_axc_ha_start_rmp_akt")

  BMW_MOD_Mxk_AxcSpFild(BMWtqw_tqw_AxcDtHaMaxDyn_sw, BMWtqw_tqw_AxcDtHaMinDyn_sw, Md_rad_fzdyn_int, BMWtqw_tqw_AxcEaSpFildPre_sw, BMWtqw_tqw_AxcHaLpaSpFildPre_sw, BMWtqw_tqw_AxcHaSpFildPre_sw, B_axc_ha_start_rmp_akt)
}


def BMW_MOD_Mxk_AxcSpUnf(BMWtqw_tqw_AxcDtHaMaxStat_sw: InMeasurement, BMWtqw_tqw_AxcDtHaMinStat_sw: InMeasurement, Md_rad_wunsch: InMeasurement, BMWtqw_tqw_AxcHaSpUnf_sw: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_Mxk_AxcSpUnf(a2lBin: A2LBinAdapter): Unit = {


val BMWtqw_tqw_AxcDtHaMaxStat_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcDtHaMaxStat_sw")
val BMWtqw_tqw_AxcDtHaMinStat_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcDtHaMinStat_sw")
val Md_rad_wunsch: InMeasurement = a2lBin.measurement("Md_rad_wunsch")
val BMWtqw_tqw_AxcHaSpUnf_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcHaSpUnf_sw")

  BMW_MOD_Mxk_AxcSpUnf(BMWtqw_tqw_AxcDtHaMaxStat_sw, BMWtqw_tqw_AxcDtHaMinStat_sw, Md_rad_wunsch, BMWtqw_tqw_AxcHaSpUnf_sw)
}


def BMW_MOD_Mxk_DmdSerlCrp(K_MD_RAD_KRIECHEN_GB1_LL_PLAUS: BigDecimal, S_MD_RAD_KRIECHEN_GB1_LL_PLAUS: String, S_MXK_DMD_SERL_CRP_EW_AKTIV: String, Status_ks_gb1_soll_mxk: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_Mxk_DmdSerlCrp(a2lBin: A2LBinAdapter): Unit = {

val K_MD_RAD_KRIECHEN_GB1_LL_PLAUS: BigDecimal = a2lBin.readCharacteristicWithCast("K_MD_RAD_KRIECHEN_GB1_LL_PLAUS")
val S_MD_RAD_KRIECHEN_GB1_LL_PLAUS: String = a2lBin.readCharacteristicWithCast("S_MD_RAD_KRIECHEN_GB1_LL_PLAUS")
val S_MXK_DMD_SERL_CRP_EW_AKTIV: String = a2lBin.readCharacteristicWithCast("S_MXK_DMD_SERL_CRP_EW_AKTIV")

val Status_ks_gb1_soll_mxk: OutMeasurement = a2lBin.measurement("Status_ks_gb1_soll_mxk")

  BMW_MOD_Mxk_DmdSerlCrp(K_MD_RAD_KRIECHEN_GB1_LL_PLAUS, S_MD_RAD_KRIECHEN_GB1_LL_PLAUS, S_MXK_DMD_SERL_CRP_EW_AKTIV, Status_ks_gb1_soll_mxk)
}


def BMW_MOD_Mxk_LdcGraHa(BMWtqw_tqw_AxcDtHaMaxDyn_sw: InMeasurement, BMWtqw_tqw_AxcDtHaMaxStat_sw: InMeasurement, BMWtqw_tqw_AxcDtHaMinStat_sw: InMeasurement, Md_rad_fzdyn_int: InMeasurement, S_NOCP349030: String, BMWtqw_tqw_AxcHaLdcSpFildPEfR_sw: OutMeasurement, BMWtqw_tqw_AxcHaLdcSpFildPre_sw: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_Mxk_LdcGraHa(a2lBin: A2LBinAdapter): Unit = {

val S_NOCP349030: String = a2lBin.readCharacteristicWithCast("S_NOCP349030")
val BMWtqw_tqw_AxcDtHaMaxDyn_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcDtHaMaxDyn_sw")
val BMWtqw_tqw_AxcDtHaMaxStat_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcDtHaMaxStat_sw")
val BMWtqw_tqw_AxcDtHaMinStat_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcDtHaMinStat_sw")
val Md_rad_fzdyn_int: InMeasurement = a2lBin.measurement("Md_rad_fzdyn_int")
val BMWtqw_tqw_AxcHaLdcSpFildPEfR_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcHaLdcSpFildPEfR_sw")
val BMWtqw_tqw_AxcHaLdcSpFildPre_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcHaLdcSpFildPre_sw")

  BMW_MOD_Mxk_LdcGraHa(BMWtqw_tqw_AxcDtHaMaxDyn_sw, BMWtqw_tqw_AxcDtHaMaxStat_sw, BMWtqw_tqw_AxcDtHaMinStat_sw, Md_rad_fzdyn_int, S_NOCP349030, BMWtqw_tqw_AxcHaLdcSpFildPEfR_sw, BMWtqw_tqw_AxcHaLdcSpFildPre_sw)
}


def BMW_MOD_Mxk_LimAxcDrDy(BMWtqw_swi_CfgVeh2DtEa_C: BigDecimal, BMWtqw_swi_CfgVeh2DtHa_C: BigDecimal): Unit = {
 ???
}

def BMW_MOD_Mxk_LimAxcDrDy(a2lBin: A2LBinAdapter): Unit = {

val BMWtqw_swi_CfgVeh2DtEa_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_swi_CfgVeh2DtEa_C")
val BMWtqw_swi_CfgVeh2DtHa_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_swi_CfgVeh2DtHa_C")



  BMW_MOD_Mxk_LimAxcDrDy(BMWtqw_swi_CfgVeh2DtEa_C, BMWtqw_swi_CfgVeh2DtHa_C)
}


def BMW_MOD_Mxk_LimAxcDrv(BMWtqw_tqw_DtHaMaxDyn_sw: InMeasurement, BMWtqw_tqw_DtHaMaxStat_sw: InMeasurement, BMWtqw_tqw_DtHaMinDyn_sw: InMeasurement, BMWtqw_tqw_DtHaMinStat_sw: InMeasurement, BMWtqw_swi_AxcCfgDynLimHa_C: BigDecimal, BMWtqw_tqw_AxcDtHaMaxDyn_sw: OutMeasurement, BMWtqw_tqw_AxcDtHaMaxStat_sw: OutMeasurement, BMWtqw_tqw_AxcDtHaMinDyn_sw: OutMeasurement, BMWtqw_tqw_AxcDtHaMinStat_sw: OutMeasurement, BMWtqw_tqw_AxcSumMaxDyn_sw: OutMeasurement, BMWtqw_tqw_AxcSumMaxStat_sw: OutMeasurement, BMWtqw_tqw_AxcSumMinDyn_sw: OutMeasurement, BMWtqw_tqw_AxcSumMinStat_sw: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_Mxk_LimAxcDrv(a2lBin: A2LBinAdapter): Unit = {

val BMWtqw_swi_AxcCfgDynLimHa_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_swi_AxcCfgDynLimHa_C")
val BMWtqw_tqw_DtHaMaxDyn_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_DtHaMaxDyn_sw")
val BMWtqw_tqw_DtHaMaxStat_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_DtHaMaxStat_sw")
val BMWtqw_tqw_DtHaMinDyn_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_DtHaMinDyn_sw")
val BMWtqw_tqw_DtHaMinStat_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_DtHaMinStat_sw")
val BMWtqw_tqw_AxcDtHaMaxDyn_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcDtHaMaxDyn_sw")
val BMWtqw_tqw_AxcDtHaMaxStat_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcDtHaMaxStat_sw")
val BMWtqw_tqw_AxcDtHaMinDyn_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcDtHaMinDyn_sw")
val BMWtqw_tqw_AxcDtHaMinStat_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcDtHaMinStat_sw")
val BMWtqw_tqw_AxcSumMaxDyn_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcSumMaxDyn_sw")
val BMWtqw_tqw_AxcSumMaxStat_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcSumMaxStat_sw")
val BMWtqw_tqw_AxcSumMinDyn_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcSumMinDyn_sw")
val BMWtqw_tqw_AxcSumMinStat_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcSumMinStat_sw")

  BMW_MOD_Mxk_LimAxcDrv(BMWtqw_tqw_DtHaMaxDyn_sw, BMWtqw_tqw_DtHaMaxStat_sw, BMWtqw_tqw_DtHaMinDyn_sw, BMWtqw_tqw_DtHaMinStat_sw, BMWtqw_swi_AxcCfgDynLimHa_C, BMWtqw_tqw_AxcDtHaMaxDyn_sw, BMWtqw_tqw_AxcDtHaMaxStat_sw, BMWtqw_tqw_AxcDtHaMinDyn_sw, BMWtqw_tqw_AxcDtHaMinStat_sw, BMWtqw_tqw_AxcSumMaxDyn_sw, BMWtqw_tqw_AxcSumMaxStat_sw, BMWtqw_tqw_AxcSumMinDyn_sw, BMWtqw_tqw_AxcSumMinStat_sw)
}


def BMW_MOD_Mxk_StLds(CW_MXK_06: BigDecimal, Status_md_ha: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_Mxk_StLds(a2lBin: A2LBinAdapter): Unit = {

val CW_MXK_06: BigDecimal = a2lBin.readCharacteristicWithCast("CW_MXK_06")

val Status_md_ha: OutMeasurement = a2lBin.measurement("Status_md_ha")

  BMW_MOD_Mxk_StLds(CW_MXK_06, Status_md_ha)
}


def BMW_MOD_Mxk_WishMoTrac(): Unit = {
 ???
}

def BMW_MOD_Mxk_WishMoTrac(a2lBin: A2LBinAdapter): Unit = {





  BMW_MOD_Mxk_WishMoTrac()
}


def BMW_MOD_Sai_Arn(St_aval_sail_dme: InMeasurement, St_sail_drv: InMeasurement): Unit = {
 ???
}

def BMW_MOD_Sai_Arn(a2lBin: A2LBinAdapter): Unit = {


val St_aval_sail_dme: InMeasurement = a2lBin.measurement("St_aval_sail_dme")
val St_sail_drv: InMeasurement = a2lBin.measurement("St_sail_drv")


  BMW_MOD_Sai_Arn(St_aval_sail_dme, St_sail_drv)
}


def BMW_MOD_Sai_Cmn(BMWosc_acvn_ModCoa_ub: InMeasurement, BMWosc_b_deac_CoaEnv_bo: InMeasurement, BMWtqe_st_tq_LimSrc_ul: InMeasurement, Nkw: InMeasurement, Nstat: InMeasurement, Segeln_dienste: InMeasurement, St_dibs2: InMeasurement, St_dibs2_B_iber_i: InMeasurement, St_dibs2_B_iber_sys: InMeasurement, St_dibs2_B_iber_t: InMeasurement, St_dibs2_B_iber_u: InMeasurement, St_dibs2_B_iber_wkur: InMeasurement, St_dibs2_B_iber_woff: InMeasurement, St_dibs2_B_ibsnok: InMeasurement, St_dsc_can: InMeasurement, St_isgusm_out100ms: InMeasurement, St_isgusm_out100ms_B_isgusm_fpsum: InMeasurement, St_isgusm_out100ms_B_isgusm_lkl: InMeasurement, St_isgusm_out100ms_B_isgusm_modgenau: InMeasurement, St_isgusm_out100ms_B_isgusm_prednosail: InMeasurement, St_isgusm_out100ms_B_isgusm_statfg: InMeasurement, St_mdfw: InMeasurement, St_mdfw_B_sld_akt: InMeasurement, St_sail_drv: InMeasurement, St_sail_grb_plaus: InMeasurement, Status_usecase_antr: InMeasurement, Tmot: InMeasurement, Toel: InMeasurement, U_batt: InMeasurement, Var_hs: InMeasurement, CW_SEGEL_APLI_SCHALTER: BigDecimal, CW_SEGEL_FEHLER_EXT: BigDecimal, CW_SEGEL_STMDINFO_S: BigDecimal, CW_SEGEL_ST_AVAI_BIT_1: BigDecimal, CW_SEGEL_ST_AVAI_BIT_2: BigDecimal, CW_SEGEL_ST_AVAI_BIT_3: BigDecimal, CW_SEGEL_ST_AVAI_BIT_4: BigDecimal, CW_SEGEL_ST_AVAI_BIT_5: BigDecimal, CW_SEGEL_ST_AVAI_BIT_6: BigDecimal, CW_SEGEL_VERH_DSC: BigDecimal, CW_USECASE_SEGEL_UBATT: BigDecimal, KL_SEGEL_NDIFF_NO: CurveType[BigDecimal, BigDecimal], K_SEGEL_ANZAHL_EF_DIENSTE: BigDecimal, K_SEGEL_MAX_DREHZAHL_O: BigDecimal, K_SEGEL_MAX_DREHZAHL_U: BigDecimal, K_SEGEL_MAX_N_LL_ALL: BigDecimal, K_SEGEL_MIN_DREHZAHL_O: BigDecimal, K_SEGEL_MIN_DREHZAHL_U: BigDecimal, K_SEGEL_TMOT_MAXIMAL: BigDecimal, K_SEGEL_TMOT_MINDEST: BigDecimal, K_SEGEL_TOEL_MAXIMAL: BigDecimal, K_SEGEL_UEBERPRUEFUNG_DREHZAHL: BigDecimal, K_TD_SEGELVERH_UBATT: BigDecimal, K_TD_SEGEL_SHOW_CW: BigDecimal, K_UBATT_MIN_SEGELN: BigDecimal, S_K0: String, St_aval_sail_dme: OutMeasurement, St_mdsegeln_1: OutMeasurement, St_mdsegeln_1_B_kein_segeln_fahrer: OutMeasurement, St_mdsegeln_1_B_segeln_auskodiert: OutMeasurement, Stat_segel_fehler_ext: OutMeasurement, Stat_segel_verhinderer: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_Sai_Cmn(a2lBin: A2LBinAdapter): Unit = {

val CW_SEGEL_APLI_SCHALTER: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_APLI_SCHALTER")
val CW_SEGEL_FEHLER_EXT: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_FEHLER_EXT")
val CW_SEGEL_STMDINFO_S: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_STMDINFO_S")
val CW_SEGEL_ST_AVAI_BIT_1: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_ST_AVAI_BIT_1")
val CW_SEGEL_ST_AVAI_BIT_2: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_ST_AVAI_BIT_2")
val CW_SEGEL_ST_AVAI_BIT_3: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_ST_AVAI_BIT_3")
val CW_SEGEL_ST_AVAI_BIT_4: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_ST_AVAI_BIT_4")
val CW_SEGEL_ST_AVAI_BIT_5: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_ST_AVAI_BIT_5")
val CW_SEGEL_ST_AVAI_BIT_6: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_ST_AVAI_BIT_6")
val CW_SEGEL_VERH_DSC: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_VERH_DSC")
val CW_USECASE_SEGEL_UBATT: BigDecimal = a2lBin.readCharacteristicWithCast("CW_USECASE_SEGEL_UBATT")
val KL_SEGEL_NDIFF_NO: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_SEGEL_NDIFF_NO")
val K_SEGEL_ANZAHL_EF_DIENSTE: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_ANZAHL_EF_DIENSTE")
val K_SEGEL_MAX_DREHZAHL_O: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_MAX_DREHZAHL_O")
val K_SEGEL_MAX_DREHZAHL_U: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_MAX_DREHZAHL_U")
val K_SEGEL_MAX_N_LL_ALL: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_MAX_N_LL_ALL")
val K_SEGEL_MIN_DREHZAHL_O: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_MIN_DREHZAHL_O")
val K_SEGEL_MIN_DREHZAHL_U: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_MIN_DREHZAHL_U")
val K_SEGEL_TMOT_MAXIMAL: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_TMOT_MAXIMAL")
val K_SEGEL_TMOT_MINDEST: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_TMOT_MINDEST")
val K_SEGEL_TOEL_MAXIMAL: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_TOEL_MAXIMAL")
val K_SEGEL_UEBERPRUEFUNG_DREHZAHL: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_UEBERPRUEFUNG_DREHZAHL")
val K_TD_SEGELVERH_UBATT: BigDecimal = a2lBin.readCharacteristicWithCast("K_TD_SEGELVERH_UBATT")
val K_TD_SEGEL_SHOW_CW: BigDecimal = a2lBin.readCharacteristicWithCast("K_TD_SEGEL_SHOW_CW")
val K_UBATT_MIN_SEGELN: BigDecimal = a2lBin.readCharacteristicWithCast("K_UBATT_MIN_SEGELN")
val S_K0: String = a2lBin.readCharacteristicWithCast("S_K0")
val BMWosc_acvn_ModCoa_ub: InMeasurement = a2lBin.measurement("BMWosc_acvn_ModCoa_ub")
val BMWosc_b_deac_CoaEnv_bo: InMeasurement = a2lBin.measurement("BMWosc_b_deac_CoaEnv_bo")
val BMWtqe_st_tq_LimSrc_ul: InMeasurement = a2lBin.measurement("BMWtqe_st_tq_LimSrc_ul")
val Nkw: InMeasurement = a2lBin.measurement("Nkw")
val Nstat: InMeasurement = a2lBin.measurement("Nstat")
val Segeln_dienste: InMeasurement = a2lBin.measurement("Segeln_dienste")
val St_dibs2: InMeasurement = a2lBin.measurement("St_dibs2")
val St_dibs2_B_iber_i: InMeasurement = a2lBin.measurement("St_dibs2.B_iber_i")
val St_dibs2_B_iber_sys: InMeasurement = a2lBin.measurement("St_dibs2.B_iber_sys")
val St_dibs2_B_iber_t: InMeasurement = a2lBin.measurement("St_dibs2.B_iber_t")
val St_dibs2_B_iber_u: InMeasurement = a2lBin.measurement("St_dibs2.B_iber_u")
val St_dibs2_B_iber_wkur: InMeasurement = a2lBin.measurement("St_dibs2.B_iber_wkur")
val St_dibs2_B_iber_woff: InMeasurement = a2lBin.measurement("St_dibs2.B_iber_woff")
val St_dibs2_B_ibsnok: InMeasurement = a2lBin.measurement("St_dibs2.B_ibsnok")
val St_dsc_can: InMeasurement = a2lBin.measurement("St_dsc_can")
val St_isgusm_out100ms: InMeasurement = a2lBin.measurement("St_isgusm_out100ms")
val St_isgusm_out100ms_B_isgusm_fpsum: InMeasurement = a2lBin.measurement("St_isgusm_out100ms.B_isgusm_fpsum")
val St_isgusm_out100ms_B_isgusm_lkl: InMeasurement = a2lBin.measurement("St_isgusm_out100ms.B_isgusm_lkl")
val St_isgusm_out100ms_B_isgusm_modgenau: InMeasurement = a2lBin.measurement("St_isgusm_out100ms.B_isgusm_modgenau")
val St_isgusm_out100ms_B_isgusm_prednosail: InMeasurement = a2lBin.measurement("St_isgusm_out100ms.B_isgusm_prednosail")
val St_isgusm_out100ms_B_isgusm_statfg: InMeasurement = a2lBin.measurement("St_isgusm_out100ms.B_isgusm_statfg")
val St_mdfw: InMeasurement = a2lBin.measurement("St_mdfw")
val St_mdfw_B_sld_akt: InMeasurement = a2lBin.measurement("St_mdfw.B_sld_akt")
val St_sail_drv: InMeasurement = a2lBin.measurement("St_sail_drv")
val St_sail_grb_plaus: InMeasurement = a2lBin.measurement("St_sail_grb_plaus")
val Status_usecase_antr: InMeasurement = a2lBin.measurement("Status_usecase_antr")
val Tmot: InMeasurement = a2lBin.measurement("Tmot")
val Toel: InMeasurement = a2lBin.measurement("Toel")
val U_batt: InMeasurement = a2lBin.measurement("U_batt")
val Var_hs: InMeasurement = a2lBin.measurement("Var_hs")
val St_aval_sail_dme: OutMeasurement = a2lBin.measurement("St_aval_sail_dme")
val St_mdsegeln_1: OutMeasurement = a2lBin.measurement("St_mdsegeln_1")
val St_mdsegeln_1_B_kein_segeln_fahrer: OutMeasurement = a2lBin.measurement("St_mdsegeln_1.B_kein_segeln_fahrer")
val St_mdsegeln_1_B_segeln_auskodiert: OutMeasurement = a2lBin.measurement("St_mdsegeln_1.B_segeln_auskodiert")
val Stat_segel_fehler_ext: OutMeasurement = a2lBin.measurement("Stat_segel_fehler_ext")
val Stat_segel_verhinderer: OutMeasurement = a2lBin.measurement("Stat_segel_verhinderer")

  BMW_MOD_Sai_Cmn(BMWosc_acvn_ModCoa_ub, BMWosc_b_deac_CoaEnv_bo, BMWtqe_st_tq_LimSrc_ul, Nkw, Nstat, Segeln_dienste, St_dibs2, St_dibs2_B_iber_i, St_dibs2_B_iber_sys, St_dibs2_B_iber_t, St_dibs2_B_iber_u, St_dibs2_B_iber_wkur, St_dibs2_B_iber_woff, St_dibs2_B_ibsnok, St_dsc_can, St_isgusm_out100ms, St_isgusm_out100ms_B_isgusm_fpsum, St_isgusm_out100ms_B_isgusm_lkl, St_isgusm_out100ms_B_isgusm_modgenau, St_isgusm_out100ms_B_isgusm_prednosail, St_isgusm_out100ms_B_isgusm_statfg, St_mdfw, St_mdfw_B_sld_akt, St_sail_drv, St_sail_grb_plaus, Status_usecase_antr, Tmot, Toel, U_batt, Var_hs, CW_SEGEL_APLI_SCHALTER, CW_SEGEL_FEHLER_EXT, CW_SEGEL_STMDINFO_S, CW_SEGEL_ST_AVAI_BIT_1, CW_SEGEL_ST_AVAI_BIT_2, CW_SEGEL_ST_AVAI_BIT_3, CW_SEGEL_ST_AVAI_BIT_4, CW_SEGEL_ST_AVAI_BIT_5, CW_SEGEL_ST_AVAI_BIT_6, CW_SEGEL_VERH_DSC, CW_USECASE_SEGEL_UBATT, KL_SEGEL_NDIFF_NO, K_SEGEL_ANZAHL_EF_DIENSTE, K_SEGEL_MAX_DREHZAHL_O, K_SEGEL_MAX_DREHZAHL_U, K_SEGEL_MAX_N_LL_ALL, K_SEGEL_MIN_DREHZAHL_O, K_SEGEL_MIN_DREHZAHL_U, K_SEGEL_TMOT_MAXIMAL, K_SEGEL_TMOT_MINDEST, K_SEGEL_TOEL_MAXIMAL, K_SEGEL_UEBERPRUEFUNG_DREHZAHL, K_TD_SEGELVERH_UBATT, K_TD_SEGEL_SHOW_CW, K_UBATT_MIN_SEGELN, S_K0, St_aval_sail_dme, St_mdsegeln_1, St_mdsegeln_1_B_kein_segeln_fahrer, St_mdsegeln_1_B_segeln_auskodiert, Stat_segel_fehler_ext, Stat_segel_verhinderer)
}


def BMW_MOD_Sai_Conv(BMWtqe_st_tq_LimSrc_ul: InMeasurement, Mdg_soll: InMeasurement, N_schalt_dkg_plaus: InMeasurement, Pwg_ist: InMeasurement, St_sail_grb_plaus: InMeasurement, Status_mdred_egs_plaus: InMeasurement, CW_SEGEL_ABWURF_BEGRENZUNG_EGS: BigDecimal, CW_SEGEL_ABWURF_ST_MDINFO_S: BigDecimal, K_SEGEL_DMDG_RMP_UP: BigDecimal, K_SEGEL_DMD_RAMPE_EINSTIEG: BigDecimal, K_SEGEL_DMD_RAMPE_PASSIV: BigDecimal, K_SEGEL_MDK_FUSSPUNKT: BigDecimal, K_SEGEL_NSCHALT_MAX: BigDecimal, K_SEGEL_NSCHALT_MIN: BigDecimal, K_T_SEGEL_MOMENT: BigDecimal, CW_SEGEL_APLI_SCHALTER: BigDecimal, S_K0: String, Mdg_segel_einstiegsbegrenzung: OutMeasurement, St_sai_conv_bits: OutMeasurement, St_sai_conv_bits_B_drehzahlbegrenzung_segeln: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_Sai_Conv(a2lBin: A2LBinAdapter): Unit = {

val CW_SEGEL_ABWURF_BEGRENZUNG_EGS: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_ABWURF_BEGRENZUNG_EGS")
val CW_SEGEL_ABWURF_ST_MDINFO_S: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_ABWURF_ST_MDINFO_S")
val K_SEGEL_DMDG_RMP_UP: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_DMDG_RMP_UP")
val K_SEGEL_DMD_RAMPE_EINSTIEG: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_DMD_RAMPE_EINSTIEG")
val K_SEGEL_DMD_RAMPE_PASSIV: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_DMD_RAMPE_PASSIV")
val K_SEGEL_MDK_FUSSPUNKT: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_MDK_FUSSPUNKT")
val K_SEGEL_NSCHALT_MAX: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_NSCHALT_MAX")
val K_SEGEL_NSCHALT_MIN: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_NSCHALT_MIN")
val K_T_SEGEL_MOMENT: BigDecimal = a2lBin.readCharacteristicWithCast("K_T_SEGEL_MOMENT")
val CW_SEGEL_APLI_SCHALTER: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_APLI_SCHALTER")
val S_K0: String = a2lBin.readCharacteristicWithCast("S_K0")
val BMWtqe_st_tq_LimSrc_ul: InMeasurement = a2lBin.measurement("BMWtqe_st_tq_LimSrc_ul")
val Mdg_soll: InMeasurement = a2lBin.measurement("Mdg_soll")
val N_schalt_dkg_plaus: InMeasurement = a2lBin.measurement("N_schalt_dkg_plaus")
val Pwg_ist: InMeasurement = a2lBin.measurement("Pwg_ist")
val St_sail_grb_plaus: InMeasurement = a2lBin.measurement("St_sail_grb_plaus")
val Status_mdred_egs_plaus: InMeasurement = a2lBin.measurement("Status_mdred_egs_plaus")
val Mdg_segel_einstiegsbegrenzung: OutMeasurement = a2lBin.measurement("Mdg_segel_einstiegsbegrenzung")
val St_sai_conv_bits: OutMeasurement = a2lBin.measurement("St_sai_conv_bits")
val St_sai_conv_bits_B_drehzahlbegrenzung_segeln: OutMeasurement = a2lBin.measurement("St_sai_conv_bits.B_drehzahlbegrenzung_segeln")

  BMW_MOD_Sai_Conv(BMWtqe_st_tq_LimSrc_ul, Mdg_soll, N_schalt_dkg_plaus, Pwg_ist, St_sail_grb_plaus, Status_mdred_egs_plaus, CW_SEGEL_ABWURF_BEGRENZUNG_EGS, CW_SEGEL_ABWURF_ST_MDINFO_S, K_SEGEL_DMDG_RMP_UP, K_SEGEL_DMD_RAMPE_EINSTIEG, K_SEGEL_DMD_RAMPE_PASSIV, K_SEGEL_MDK_FUSSPUNKT, K_SEGEL_NSCHALT_MAX, K_SEGEL_NSCHALT_MIN, K_T_SEGEL_MOMENT, CW_SEGEL_APLI_SCHALTER, S_K0, Mdg_segel_einstiegsbegrenzung, St_sai_conv_bits, St_sai_conv_bits_B_drehzahlbegrenzung_segeln)
}


def BMW_MOD_Sai_Hyb(A_quer_abs: InMeasurement, Dm_ab_fws_plaus: InMeasurement, Dmd_rad_spaf: InMeasurement, Md_rad_brems: InMeasurement, Md_rad_fzdyn_int: InMeasurement, Md_rad_ist: InMeasurement, Md_rad_pedal: InMeasurement, Md_rad_wunsch: InMeasurement, Mdk_wunsch: InMeasurement, N_achse_antr: InMeasurement, Pwg_ist: InMeasurement, Soc_hvb_rel: InMeasurement, St_antrieb_wunsch: InMeasurement, St_aval_sail_dme: InMeasurement, St_brg_dv: InMeasurement, St_egsprog_kor: InMeasurement, St_gang: InMeasurement, St_mdsegeln_1: InMeasurement, St_mdsegeln_1_B_kein_segeln_fahrer: InMeasurement, St_mdsegeln_1_B_segeln_auskodiert: InMeasurement, St_msasegeln: InMeasurement, St_oz: InMeasurement, St_oz_B_anhang: InMeasurement, St_sail_grb_plaus: InMeasurement, Status_antrieb_ist: InMeasurement, Status_ks_gb1_ist_plaus: InMeasurement, Status_ks_gb2_ist_plaus: InMeasurement, Status_usecase_antr: InMeasurement, Status_usecase_soc: InMeasurement, V_fzg_plaus: InMeasurement, Var_hs: InMeasurement, CW_SEGEL_AKTIV_MSA: BigDecimal, CW_SEGEL_AUSSTIEG_HYB: BigDecimal, CW_SEGEL_AUSWAHL_PEDAL: BigDecimal, CW_SEGEL_BREMSEN_HYB: BigDecimal, CW_SEGEL_DEAKTIVIERER_HYB: BigDecimal, CW_SEGEL_EFAHREN: BigDecimal, CW_SEGEL_KS_GB1_EFAHREN: BigDecimal, CW_SEGEL_KS_GB1_HYBRIDISCH: BigDecimal, CW_SEGEL_ST_BRG_DV: BigDecimal, CW_SEGEL_ST_EGS_PROG: BigDecimal, CW_SEGEL_ST_GANG: BigDecimal, CW_SEGEL_ST_GANG_SAIL_GRB: BigDecimal, CW_SEGEL_ST_KS_GB2_IST: BigDecimal, CW_SEGEL_ST_USECASE_ANTRIEB: BigDecimal, CW_SEGEL_ST_USECASE_SOC: BigDecimal, CW_SEGEL_VERHINDERER_HYB: BigDecimal, CW_SEGEL_VERH_GRB_HYB: BigDecimal, KL_SEGEL_A_QUER_OFF: CurveType[BigDecimal, BigDecimal], KL_SEGEL_MD_VERZ_AUS: CurveType[BigDecimal, BigDecimal], KL_SEGEL_MD_VERZ_BREMS: CurveType[BigDecimal, BigDecimal], KT_SEGEL_UMCOD_DISP: Array[BigDecimal], K_MD_ST_SAIL_DRV_VERZ_L: BigDecimal, K_MD_ST_SAIL_DRV_VERZ_R: BigDecimal, K_MD_ST_SAIL_DRV_VERZ_UL: BigDecimal, K_MD_ST_SAIL_DRV_VERZ_UR: BigDecimal, K_SEGEL_A_QUER: BigDecimal, K_SEGEL_A_QUER_HYS: BigDecimal, K_SEGEL_DEAKTIV_MSA_HYB: BigDecimal, K_SEGEL_DRV_ERR: BigDecimal, K_SEGEL_DRV_HS: BigDecimal, K_SEGEL_MDK_E_OFS: BigDecimal, K_SEGEL_MD_BREMS_AUS: BigDecimal, K_SEGEL_MD_BREMS_HYS: BigDecimal, K_SEGEL_MD_BREMS_MAX: BigDecimal, K_SEGEL_MD_RAD_AUS: BigDecimal, K_SEGEL_MD_RAD_IST_MIN: BigDecimal, K_SEGEL_MD_RAD_PEDAL_MIN: BigDecimal, K_SEGEL_MD_RAD_WUNSCH_MIN: BigDecimal, K_SEGEL_MD_STEIG_AUS: BigDecimal, K_SEGEL_MD_STEIG_GEFAELLE_AUS: BigDecimal, K_SEGEL_MD_STEIG_GEFAELLE_BREMSE: BigDecimal, K_SEGEL_MD_STEIG_GEFAELLE_ENDE: BigDecimal, K_SEGEL_MD_STEIG_GEFAELLE_VERH: BigDecimal, K_SEGEL_MD_STEIG_HYS: BigDecimal, K_SEGEL_MD_STEIG_MAX: BigDecimal, K_SEGEL_MD_VERZ_HYS: BigDecimal, K_SEGEL_MIN_STATUS_MSA_HYB: BigDecimal, K_SEGEL_NOTWEND_MSA_HYB: BigDecimal, K_SEGEL_PWG_AUS: BigDecimal, K_SEGEL_PWG_MIN: BigDecimal, K_SEGEL_PWG_STEIG_AUS_ENDE: BigDecimal, K_SEGEL_P_AUS: BigDecimal, K_SEGEL_SOC_HYS: BigDecimal, K_SEGEL_SOC_MIN: BigDecimal, K_SEGEL_VEINST_HYS: BigDecimal, K_SEGEL_VEINST_MIN: BigDecimal, K_SEGEL_V_MAX: BigDecimal, K_SEGEL_V_MIN: BigDecimal, K_SEGEL_V_STEIG_AUS_ENDE: BigDecimal, K_TD_ST_SAIL_DRV_VERZ_SA: BigDecimal, K_TD_UC_WECHSEL_SEGEL: BigDecimal, S_ANZ_BREMS_SV: String, CW_SEGEL_APLI_SCHALTER: BigDecimal, S_K0: String, B_segel_aktiv_msa: OutMeasurement, St_sai_hyb_bits: OutMeasurement, St_sai_hyb_bits_B_segelpedal_hyb: OutMeasurement, St_sail_drv: OutMeasurement, Stat_segel_hybrid: OutMeasurement): Unit = {
 ???
}

def BMW_MOD_Sai_Hyb(a2lBin: A2LBinAdapter): Unit = {

val CW_SEGEL_AKTIV_MSA: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_AKTIV_MSA")
val CW_SEGEL_AUSSTIEG_HYB: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_AUSSTIEG_HYB")
val CW_SEGEL_AUSWAHL_PEDAL: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_AUSWAHL_PEDAL")
val CW_SEGEL_BREMSEN_HYB: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_BREMSEN_HYB")
val CW_SEGEL_DEAKTIVIERER_HYB: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_DEAKTIVIERER_HYB")
val CW_SEGEL_EFAHREN: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_EFAHREN")
val CW_SEGEL_KS_GB1_EFAHREN: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_KS_GB1_EFAHREN")
val CW_SEGEL_KS_GB1_HYBRIDISCH: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_KS_GB1_HYBRIDISCH")
val CW_SEGEL_ST_BRG_DV: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_ST_BRG_DV")
val CW_SEGEL_ST_EGS_PROG: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_ST_EGS_PROG")
val CW_SEGEL_ST_GANG: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_ST_GANG")
val CW_SEGEL_ST_GANG_SAIL_GRB: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_ST_GANG_SAIL_GRB")
val CW_SEGEL_ST_KS_GB2_IST: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_ST_KS_GB2_IST")
val CW_SEGEL_ST_USECASE_ANTRIEB: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_ST_USECASE_ANTRIEB")
val CW_SEGEL_ST_USECASE_SOC: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_ST_USECASE_SOC")
val CW_SEGEL_VERHINDERER_HYB: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_VERHINDERER_HYB")
val CW_SEGEL_VERH_GRB_HYB: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_VERH_GRB_HYB")
val KL_SEGEL_A_QUER_OFF: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_SEGEL_A_QUER_OFF")
val KL_SEGEL_MD_VERZ_AUS: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_SEGEL_MD_VERZ_AUS")
val KL_SEGEL_MD_VERZ_BREMS: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_SEGEL_MD_VERZ_BREMS")
val KT_SEGEL_UMCOD_DISP: Array[BigDecimal] = a2lBin.readCharacteristicWithCast("KT_SEGEL_UMCOD_DISP")
val K_MD_ST_SAIL_DRV_VERZ_L: BigDecimal = a2lBin.readCharacteristicWithCast("K_MD_ST_SAIL_DRV_VERZ_L")
val K_MD_ST_SAIL_DRV_VERZ_R: BigDecimal = a2lBin.readCharacteristicWithCast("K_MD_ST_SAIL_DRV_VERZ_R")
val K_MD_ST_SAIL_DRV_VERZ_UL: BigDecimal = a2lBin.readCharacteristicWithCast("K_MD_ST_SAIL_DRV_VERZ_UL")
val K_MD_ST_SAIL_DRV_VERZ_UR: BigDecimal = a2lBin.readCharacteristicWithCast("K_MD_ST_SAIL_DRV_VERZ_UR")
val K_SEGEL_A_QUER: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_A_QUER")
val K_SEGEL_A_QUER_HYS: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_A_QUER_HYS")
val K_SEGEL_DEAKTIV_MSA_HYB: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_DEAKTIV_MSA_HYB")
val K_SEGEL_DRV_ERR: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_DRV_ERR")
val K_SEGEL_DRV_HS: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_DRV_HS")
val K_SEGEL_MDK_E_OFS: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_MDK_E_OFS")
val K_SEGEL_MD_BREMS_AUS: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_MD_BREMS_AUS")
val K_SEGEL_MD_BREMS_HYS: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_MD_BREMS_HYS")
val K_SEGEL_MD_BREMS_MAX: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_MD_BREMS_MAX")
val K_SEGEL_MD_RAD_AUS: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_MD_RAD_AUS")
val K_SEGEL_MD_RAD_IST_MIN: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_MD_RAD_IST_MIN")
val K_SEGEL_MD_RAD_PEDAL_MIN: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_MD_RAD_PEDAL_MIN")
val K_SEGEL_MD_RAD_WUNSCH_MIN: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_MD_RAD_WUNSCH_MIN")
val K_SEGEL_MD_STEIG_AUS: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_MD_STEIG_AUS")
val K_SEGEL_MD_STEIG_GEFAELLE_AUS: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_MD_STEIG_GEFAELLE_AUS")
val K_SEGEL_MD_STEIG_GEFAELLE_BREMSE: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_MD_STEIG_GEFAELLE_BREMSE")
val K_SEGEL_MD_STEIG_GEFAELLE_ENDE: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_MD_STEIG_GEFAELLE_ENDE")
val K_SEGEL_MD_STEIG_GEFAELLE_VERH: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_MD_STEIG_GEFAELLE_VERH")
val K_SEGEL_MD_STEIG_HYS: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_MD_STEIG_HYS")
val K_SEGEL_MD_STEIG_MAX: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_MD_STEIG_MAX")
val K_SEGEL_MD_VERZ_HYS: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_MD_VERZ_HYS")
val K_SEGEL_MIN_STATUS_MSA_HYB: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_MIN_STATUS_MSA_HYB")
val K_SEGEL_NOTWEND_MSA_HYB: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_NOTWEND_MSA_HYB")
val K_SEGEL_PWG_AUS: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_PWG_AUS")
val K_SEGEL_PWG_MIN: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_PWG_MIN")
val K_SEGEL_PWG_STEIG_AUS_ENDE: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_PWG_STEIG_AUS_ENDE")
val K_SEGEL_P_AUS: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_P_AUS")
val K_SEGEL_SOC_HYS: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_SOC_HYS")
val K_SEGEL_SOC_MIN: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_SOC_MIN")
val K_SEGEL_VEINST_HYS: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_VEINST_HYS")
val K_SEGEL_VEINST_MIN: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_VEINST_MIN")
val K_SEGEL_V_MAX: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_V_MAX")
val K_SEGEL_V_MIN: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_V_MIN")
val K_SEGEL_V_STEIG_AUS_ENDE: BigDecimal = a2lBin.readCharacteristicWithCast("K_SEGEL_V_STEIG_AUS_ENDE")
val K_TD_ST_SAIL_DRV_VERZ_SA: BigDecimal = a2lBin.readCharacteristicWithCast("K_TD_ST_SAIL_DRV_VERZ_SA")
val K_TD_UC_WECHSEL_SEGEL: BigDecimal = a2lBin.readCharacteristicWithCast("K_TD_UC_WECHSEL_SEGEL")
val S_ANZ_BREMS_SV: String = a2lBin.readCharacteristicWithCast("S_ANZ_BREMS_SV")
val CW_SEGEL_APLI_SCHALTER: BigDecimal = a2lBin.readCharacteristicWithCast("CW_SEGEL_APLI_SCHALTER")
val S_K0: String = a2lBin.readCharacteristicWithCast("S_K0")
val A_quer_abs: InMeasurement = a2lBin.measurement("A_quer_abs")
val Dm_ab_fws_plaus: InMeasurement = a2lBin.measurement("Dm_ab_fws_plaus")
val Dmd_rad_spaf: InMeasurement = a2lBin.measurement("Dmd_rad_spaf")
val Md_rad_brems: InMeasurement = a2lBin.measurement("Md_rad_brems")
val Md_rad_fzdyn_int: InMeasurement = a2lBin.measurement("Md_rad_fzdyn_int")
val Md_rad_ist: InMeasurement = a2lBin.measurement("Md_rad_ist")
val Md_rad_pedal: InMeasurement = a2lBin.measurement("Md_rad_pedal")
val Md_rad_wunsch: InMeasurement = a2lBin.measurement("Md_rad_wunsch")
val Mdk_wunsch: InMeasurement = a2lBin.measurement("Mdk_wunsch")
val N_achse_antr: InMeasurement = a2lBin.measurement("N_achse_antr")
val Pwg_ist: InMeasurement = a2lBin.measurement("Pwg_ist")
val Soc_hvb_rel: InMeasurement = a2lBin.measurement("Soc_hvb_rel")
val St_antrieb_wunsch: InMeasurement = a2lBin.measurement("St_antrieb_wunsch")
val St_aval_sail_dme: InMeasurement = a2lBin.measurement("St_aval_sail_dme")
val St_brg_dv: InMeasurement = a2lBin.measurement("St_brg_dv")
val St_egsprog_kor: InMeasurement = a2lBin.measurement("St_egsprog_kor")
val St_gang: InMeasurement = a2lBin.measurement("St_gang")
val St_mdsegeln_1: InMeasurement = a2lBin.measurement("St_mdsegeln_1")
val St_mdsegeln_1_B_kein_segeln_fahrer: InMeasurement = a2lBin.measurement("St_mdsegeln_1.B_kein_segeln_fahrer")
val St_mdsegeln_1_B_segeln_auskodiert: InMeasurement = a2lBin.measurement("St_mdsegeln_1.B_segeln_auskodiert")
val St_msasegeln: InMeasurement = a2lBin.measurement("St_msasegeln")
val St_oz: InMeasurement = a2lBin.measurement("St_oz")
val St_oz_B_anhang: InMeasurement = a2lBin.measurement("St_oz.B_anhang")
val St_sail_grb_plaus: InMeasurement = a2lBin.measurement("St_sail_grb_plaus")
val Status_antrieb_ist: InMeasurement = a2lBin.measurement("Status_antrieb_ist")
val Status_ks_gb1_ist_plaus: InMeasurement = a2lBin.measurement("Status_ks_gb1_ist_plaus")
val Status_ks_gb2_ist_plaus: InMeasurement = a2lBin.measurement("Status_ks_gb2_ist_plaus")
val Status_usecase_antr: InMeasurement = a2lBin.measurement("Status_usecase_antr")
val Status_usecase_soc: InMeasurement = a2lBin.measurement("Status_usecase_soc")
val V_fzg_plaus: InMeasurement = a2lBin.measurement("V_fzg_plaus")
val Var_hs: InMeasurement = a2lBin.measurement("Var_hs")
val B_segel_aktiv_msa: OutMeasurement = a2lBin.measurement("B_segel_aktiv_msa")
val St_sai_hyb_bits: OutMeasurement = a2lBin.measurement("St_sai_hyb_bits")
val St_sai_hyb_bits_B_segelpedal_hyb: OutMeasurement = a2lBin.measurement("St_sai_hyb_bits.B_segelpedal_hyb")
val St_sail_drv: OutMeasurement = a2lBin.measurement("St_sail_drv")
val Stat_segel_hybrid: OutMeasurement = a2lBin.measurement("Stat_segel_hybrid")

  BMW_MOD_Sai_Hyb(A_quer_abs, Dm_ab_fws_plaus, Dmd_rad_spaf, Md_rad_brems, Md_rad_fzdyn_int, Md_rad_ist, Md_rad_pedal, Md_rad_wunsch, Mdk_wunsch, N_achse_antr, Pwg_ist, Soc_hvb_rel, St_antrieb_wunsch, St_aval_sail_dme, St_brg_dv, St_egsprog_kor, St_gang, St_mdsegeln_1, St_mdsegeln_1_B_kein_segeln_fahrer, St_mdsegeln_1_B_segeln_auskodiert, St_msasegeln, St_oz, St_oz_B_anhang, St_sail_grb_plaus, Status_antrieb_ist, Status_ks_gb1_ist_plaus, Status_ks_gb2_ist_plaus, Status_usecase_antr, Status_usecase_soc, V_fzg_plaus, Var_hs, CW_SEGEL_AKTIV_MSA, CW_SEGEL_AUSSTIEG_HYB, CW_SEGEL_AUSWAHL_PEDAL, CW_SEGEL_BREMSEN_HYB, CW_SEGEL_DEAKTIVIERER_HYB, CW_SEGEL_EFAHREN, CW_SEGEL_KS_GB1_EFAHREN, CW_SEGEL_KS_GB1_HYBRIDISCH, CW_SEGEL_ST_BRG_DV, CW_SEGEL_ST_EGS_PROG, CW_SEGEL_ST_GANG, CW_SEGEL_ST_GANG_SAIL_GRB, CW_SEGEL_ST_KS_GB2_IST, CW_SEGEL_ST_USECASE_ANTRIEB, CW_SEGEL_ST_USECASE_SOC, CW_SEGEL_VERHINDERER_HYB, CW_SEGEL_VERH_GRB_HYB, KL_SEGEL_A_QUER_OFF, KL_SEGEL_MD_VERZ_AUS, KL_SEGEL_MD_VERZ_BREMS, KT_SEGEL_UMCOD_DISP, K_MD_ST_SAIL_DRV_VERZ_L, K_MD_ST_SAIL_DRV_VERZ_R, K_MD_ST_SAIL_DRV_VERZ_UL, K_MD_ST_SAIL_DRV_VERZ_UR, K_SEGEL_A_QUER, K_SEGEL_A_QUER_HYS, K_SEGEL_DEAKTIV_MSA_HYB, K_SEGEL_DRV_ERR, K_SEGEL_DRV_HS, K_SEGEL_MDK_E_OFS, K_SEGEL_MD_BREMS_AUS, K_SEGEL_MD_BREMS_HYS, K_SEGEL_MD_BREMS_MAX, K_SEGEL_MD_RAD_AUS, K_SEGEL_MD_RAD_IST_MIN, K_SEGEL_MD_RAD_PEDAL_MIN, K_SEGEL_MD_RAD_WUNSCH_MIN, K_SEGEL_MD_STEIG_AUS, K_SEGEL_MD_STEIG_GEFAELLE_AUS, K_SEGEL_MD_STEIG_GEFAELLE_BREMSE, K_SEGEL_MD_STEIG_GEFAELLE_ENDE, K_SEGEL_MD_STEIG_GEFAELLE_VERH, K_SEGEL_MD_STEIG_HYS, K_SEGEL_MD_STEIG_MAX, K_SEGEL_MD_VERZ_HYS, K_SEGEL_MIN_STATUS_MSA_HYB, K_SEGEL_NOTWEND_MSA_HYB, K_SEGEL_PWG_AUS, K_SEGEL_PWG_MIN, K_SEGEL_PWG_STEIG_AUS_ENDE, K_SEGEL_P_AUS, K_SEGEL_SOC_HYS, K_SEGEL_SOC_MIN, K_SEGEL_VEINST_HYS, K_SEGEL_VEINST_MIN, K_SEGEL_V_MAX, K_SEGEL_V_MIN, K_SEGEL_V_STEIG_AUS_ENDE, K_TD_ST_SAIL_DRV_VERZ_SA, K_TD_UC_WECHSEL_SEGEL, S_ANZ_BREMS_SV, CW_SEGEL_APLI_SCHALTER, S_K0, B_segel_aktiv_msa, St_sai_hyb_bits, St_sai_hyb_bits_B_segelpedal_hyb, St_sail_drv, Stat_segel_hybrid)
}


def P_MADMK_10ms(BMWmsa_b_TankEmpTqLim_bo: InMeasurement, BMWmsa_vol_FuTank_uw: InMeasurement, BMWosc_n_K0InMax_sw: InMeasurement, BMWosc_v_VehMax_uw: InMeasurement, BMWtqw_tqw_DtHaAvlStatIsc_sw: InMeasurement, BMWtqw_tqw_DtHaMinStatIsc_sw: InMeasurement, Brtorqsum_plaus: InMeasurement, Dm_ab_fws_begr: InMeasurement, I_ges: InMeasurement, I_ges_vh: InMeasurement, Md_fzg_max_lhm: InMeasurement, Md_rad_fwst: InMeasurement, Md_rad_fzdyn_getr: InMeasurement, Md_rad_fzdyn_vb: InMeasurement, Md_rad_ist: InMeasurement, Md_rad_max_emf: InMeasurement, Md_rad_min_zka: InMeasurement, Md_rad_wunsch_vb: InMeasurement, N_rad_haxl: InMeasurement, Neig_l_plaus: InMeasurement, Nkw: InMeasurement, Nkw_ref: InMeasurement, Pwg_ist_mafw: InMeasurement, St_rcog_fshup: InMeasurement, V_fzg_fahrtricht_max: InMeasurement, V_fzg_max_lhm: InMeasurement, V_mafw: InMeasurement, KF_KP_VLIM: MapType[BigDecimal, BigDecimal, BigDecimal], KF_KP_VLIM_NKW: MapType[BigDecimal, BigDecimal, BigDecimal], KF_KP_VLIM_RUECK: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MD_RAD_DYN_VORST_FZDYN: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MD_RAD_DYN_VORST_WUNSCH: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MD_RAD_MIN_EM: MapType[BigDecimal, BigDecimal, BigDecimal], KL_MD_RAD_MAX_TANK: CurveType[BigDecimal, BigDecimal], KL_NMAX_OFFSET: CurveType[BigDecimal, BigDecimal], KL_OMEGA0_VLIM: CurveType[BigDecimal, BigDecimal], KL_PWR_MAX_TANK: CurveType[BigDecimal, BigDecimal], KL_STEIG_AKT: CurveType[BigDecimal, BigDecimal], KL_VLIM_FILTER: CurveType[BigDecimal, BigDecimal], K_DELTA_VLIM_MAX: BigDecimal, K_DELTA_VLIM_MIN: BigDecimal, K_ENA_STOERKRAFTSCHAETZER: BigDecimal, K_FK_VOL_FU_TANK: BigDecimal, K_GRD_MD_RAD_MAX_TANK_DN: BigDecimal, K_GRD_MD_RAD_MAX_TANK_UP: BigDecimal, K_KP_VLIM: BigDecimal, K_MD_RAD_WUNSCH_MAX: BigDecimal, K_M_SW_VLIM: BigDecimal, K_R_SW_VLIM: BigDecimal, K_V2NAB: BigDecimal, K_VLIM_APPLI: BigDecimal, S_MADMK_DEAKT: String, S_MD_RAD_FZDYN: String, S_SPT65_CP353348: String, S_TEST_MD_RAD_MAX_TANK: String, S_VLIM_APPLI_V: BigDecimal, S_VLIM_NKW: String, S_ZWANGSSCHALT_AKT: String, S_ZWANGSSCHALT_BYP: String, CW_NEDI_LLRADD_CAN: Array[String], CW_NEDI_LLR_CAN: Array[String], Md_rad_fzdyn: OutMeasurement, Md_rad_fzdyn_int: OutMeasurement, Md_rad_wunsch: OutMeasurement, St_mdinfo_madmk: OutMeasurement, St_mdinfo_madmk_B_emf_lim: OutMeasurement, St_mdinfo_madmk_B_steigbegr_lim: OutMeasurement, St_mdinfo_madmk_B_vlim_lim: OutMeasurement): Unit = {
 ???
}

def P_MADMK_10ms(a2lBin: A2LBinAdapter): Unit = {

val KF_KP_VLIM: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_KP_VLIM")
val KF_KP_VLIM_NKW: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_KP_VLIM_NKW")
val KF_KP_VLIM_RUECK: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_KP_VLIM_RUECK")
val KF_MD_RAD_DYN_VORST_FZDYN: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MD_RAD_DYN_VORST_FZDYN")
val KF_MD_RAD_DYN_VORST_WUNSCH: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MD_RAD_DYN_VORST_WUNSCH")
val KF_MD_RAD_MIN_EM: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MD_RAD_MIN_EM")
val KL_MD_RAD_MAX_TANK: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MD_RAD_MAX_TANK")
val KL_NMAX_OFFSET: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_NMAX_OFFSET")
val KL_OMEGA0_VLIM: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_OMEGA0_VLIM")
val KL_PWR_MAX_TANK: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_PWR_MAX_TANK")
val KL_STEIG_AKT: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_STEIG_AKT")
val KL_VLIM_FILTER: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_VLIM_FILTER")
val K_DELTA_VLIM_MAX: BigDecimal = a2lBin.readCharacteristicWithCast("K_DELTA_VLIM_MAX")
val K_DELTA_VLIM_MIN: BigDecimal = a2lBin.readCharacteristicWithCast("K_DELTA_VLIM_MIN")
val K_ENA_STOERKRAFTSCHAETZER: BigDecimal = a2lBin.readCharacteristicWithCast("K_ENA_STOERKRAFTSCHAETZER")
val K_FK_VOL_FU_TANK: BigDecimal = a2lBin.readCharacteristicWithCast("K_FK_VOL_FU_TANK")
val K_GRD_MD_RAD_MAX_TANK_DN: BigDecimal = a2lBin.readCharacteristicWithCast("K_GRD_MD_RAD_MAX_TANK_DN")
val K_GRD_MD_RAD_MAX_TANK_UP: BigDecimal = a2lBin.readCharacteristicWithCast("K_GRD_MD_RAD_MAX_TANK_UP")
val K_KP_VLIM: BigDecimal = a2lBin.readCharacteristicWithCast("K_KP_VLIM")
val K_MD_RAD_WUNSCH_MAX: BigDecimal = a2lBin.readCharacteristicWithCast("K_MD_RAD_WUNSCH_MAX")
val K_M_SW_VLIM: BigDecimal = a2lBin.readCharacteristicWithCast("K_M_SW_VLIM")
val K_R_SW_VLIM: BigDecimal = a2lBin.readCharacteristicWithCast("K_R_SW_VLIM")
val K_V2NAB: BigDecimal = a2lBin.readCharacteristicWithCast("K_V2NAB")
val K_VLIM_APPLI: BigDecimal = a2lBin.readCharacteristicWithCast("K_VLIM_APPLI")
val S_MADMK_DEAKT: String = a2lBin.readCharacteristicWithCast("S_MADMK_DEAKT")
val S_MD_RAD_FZDYN: String = a2lBin.readCharacteristicWithCast("S_MD_RAD_FZDYN")
val S_SPT65_CP353348: String = a2lBin.readCharacteristicWithCast("S_SPT65_CP353348")
val S_TEST_MD_RAD_MAX_TANK: String = a2lBin.readCharacteristicWithCast("S_TEST_MD_RAD_MAX_TANK")
val S_VLIM_APPLI_V: BigDecimal = a2lBin.readCharacteristicWithCast("S_VLIM_APPLI_V")
val S_VLIM_NKW: String = a2lBin.readCharacteristicWithCast("S_VLIM_NKW")
val S_ZWANGSSCHALT_AKT: String = a2lBin.readCharacteristicWithCast("S_ZWANGSSCHALT_AKT")
val S_ZWANGSSCHALT_BYP: String = a2lBin.readCharacteristicWithCast("S_ZWANGSSCHALT_BYP")
val CW_NEDI_LLRADD_CAN: Array[String] = a2lBin.readCharacteristicWithCast("CW_NEDI_LLRADD_CAN")
val CW_NEDI_LLR_CAN: Array[String] = a2lBin.readCharacteristicWithCast("CW_NEDI_LLR_CAN")
val BMWmsa_b_TankEmpTqLim_bo: InMeasurement = a2lBin.measurement("BMWmsa_b_TankEmpTqLim_bo")
val BMWmsa_vol_FuTank_uw: InMeasurement = a2lBin.measurement("BMWmsa_vol_FuTank_uw")
val BMWosc_n_K0InMax_sw: InMeasurement = a2lBin.measurement("BMWosc_n_K0InMax_sw")
val BMWosc_v_VehMax_uw: InMeasurement = a2lBin.measurement("BMWosc_v_VehMax_uw")
val BMWtqw_tqw_DtHaAvlStatIsc_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_DtHaAvlStatIsc_sw")
val BMWtqw_tqw_DtHaMinStatIsc_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_DtHaMinStatIsc_sw")
val Brtorqsum_plaus: InMeasurement = a2lBin.measurement("Brtorqsum_plaus")
val Dm_ab_fws_begr: InMeasurement = a2lBin.measurement("Dm_ab_fws_begr")
val I_ges: InMeasurement = a2lBin.measurement("I_ges")
val I_ges_vh: InMeasurement = a2lBin.measurement("I_ges_vh")
val Md_fzg_max_lhm: InMeasurement = a2lBin.measurement("Md_fzg_max_lhm")
val Md_rad_fwst: InMeasurement = a2lBin.measurement("Md_rad_fwst")
val Md_rad_fzdyn_getr: InMeasurement = a2lBin.measurement("Md_rad_fzdyn_getr")
val Md_rad_fzdyn_vb: InMeasurement = a2lBin.measurement("Md_rad_fzdyn_vb")
val Md_rad_ist: InMeasurement = a2lBin.measurement("Md_rad_ist")
val Md_rad_max_emf: InMeasurement = a2lBin.measurement("Md_rad_max_emf")
val Md_rad_min_zka: InMeasurement = a2lBin.measurement("Md_rad_min_zka")
val Md_rad_wunsch_vb: InMeasurement = a2lBin.measurement("Md_rad_wunsch_vb")
val N_rad_haxl: InMeasurement = a2lBin.measurement("N_rad_haxl")
val Neig_l_plaus: InMeasurement = a2lBin.measurement("Neig_l_plaus")
val Nkw: InMeasurement = a2lBin.measurement("Nkw")
val Nkw_ref: InMeasurement = a2lBin.measurement("Nkw_ref")
val Pwg_ist_mafw: InMeasurement = a2lBin.measurement("Pwg_ist_mafw")
val St_rcog_fshup: InMeasurement = a2lBin.measurement("St_rcog_fshup")
val V_fzg_fahrtricht_max: InMeasurement = a2lBin.measurement("V_fzg_fahrtricht_max")
val V_fzg_max_lhm: InMeasurement = a2lBin.measurement("V_fzg_max_lhm")
val V_mafw: InMeasurement = a2lBin.measurement("V_mafw")
val Md_rad_fzdyn: OutMeasurement = a2lBin.measurement("Md_rad_fzdyn")
val Md_rad_fzdyn_int: OutMeasurement = a2lBin.measurement("Md_rad_fzdyn_int")
val Md_rad_wunsch: OutMeasurement = a2lBin.measurement("Md_rad_wunsch")
val St_mdinfo_madmk: OutMeasurement = a2lBin.measurement("St_mdinfo_madmk")
val St_mdinfo_madmk_B_emf_lim: OutMeasurement = a2lBin.measurement("St_mdinfo_madmk.B_emf_lim")
val St_mdinfo_madmk_B_steigbegr_lim: OutMeasurement = a2lBin.measurement("St_mdinfo_madmk.B_steigbegr_lim")
val St_mdinfo_madmk_B_vlim_lim: OutMeasurement = a2lBin.measurement("St_mdinfo_madmk.B_vlim_lim")

  P_MADMK_10ms(BMWmsa_b_TankEmpTqLim_bo, BMWmsa_vol_FuTank_uw, BMWosc_n_K0InMax_sw, BMWosc_v_VehMax_uw, BMWtqw_tqw_DtHaAvlStatIsc_sw, BMWtqw_tqw_DtHaMinStatIsc_sw, Brtorqsum_plaus, Dm_ab_fws_begr, I_ges, I_ges_vh, Md_fzg_max_lhm, Md_rad_fwst, Md_rad_fzdyn_getr, Md_rad_fzdyn_vb, Md_rad_ist, Md_rad_max_emf, Md_rad_min_zka, Md_rad_wunsch_vb, N_rad_haxl, Neig_l_plaus, Nkw, Nkw_ref, Pwg_ist_mafw, St_rcog_fshup, V_fzg_fahrtricht_max, V_fzg_max_lhm, V_mafw, KF_KP_VLIM, KF_KP_VLIM_NKW, KF_KP_VLIM_RUECK, KF_MD_RAD_DYN_VORST_FZDYN, KF_MD_RAD_DYN_VORST_WUNSCH, KF_MD_RAD_MIN_EM, KL_MD_RAD_MAX_TANK, KL_NMAX_OFFSET, KL_OMEGA0_VLIM, KL_PWR_MAX_TANK, KL_STEIG_AKT, KL_VLIM_FILTER, K_DELTA_VLIM_MAX, K_DELTA_VLIM_MIN, K_ENA_STOERKRAFTSCHAETZER, K_FK_VOL_FU_TANK, K_GRD_MD_RAD_MAX_TANK_DN, K_GRD_MD_RAD_MAX_TANK_UP, K_KP_VLIM, K_MD_RAD_WUNSCH_MAX, K_M_SW_VLIM, K_R_SW_VLIM, K_V2NAB, K_VLIM_APPLI, S_MADMK_DEAKT, S_MD_RAD_FZDYN, S_SPT65_CP353348, S_TEST_MD_RAD_MAX_TANK, S_VLIM_APPLI_V, S_VLIM_NKW, S_ZWANGSSCHALT_AKT, S_ZWANGSSCHALT_BYP, CW_NEDI_LLRADD_CAN, CW_NEDI_LLR_CAN, Md_rad_fzdyn, Md_rad_fzdyn_int, Md_rad_wunsch, St_mdinfo_madmk, St_mdinfo_madmk_B_emf_lim, St_mdinfo_madmk_B_steigbegr_lim, St_mdinfo_madmk_B_vlim_lim)
}


def P_MAFAS_10ms(BMWbdy_b_CluOp90_bo: InMeasurement, Md_rad_min_zka: InMeasurement, Md_rad_schlepp: InMeasurement, Md_rad_schlepp_soll: InMeasurement, Md_rad_soll: InMeasurement, Md_rad_wunsch: InMeasurement, St_anman1: InMeasurement, St_anman1_B_ldm_ena: InMeasurement, St_anman1_B_nosa: InMeasurement, St_fas_mradsoll: InMeasurement, St_ldm_kupp: InMeasurement, St_mdipmfw: InMeasurement, St_mdipmfw_B_bst: InMeasurement, St_mdipmfw_B_emf_aktiv: InMeasurement, St_mdipmfw_B_reku: InMeasurement, St_mdipmfw_B_schlepp: InMeasurement, Var_hs: InMeasurement, KL_MAFAS_LDM_BREMS: CurveType[BigDecimal, BigDecimal], K_MAFAS_KEIN_KS: BigDecimal, K_MAFAS_MD_NAHE_DTORQ_BOT: BigDecimal, K_MAFAS_RAMP_DCC_OFF: BigDecimal, K_MAFAS_RAMP_SLD_ERR: BigDecimal, K_MAFAS_RAMP_SLD_OFF: BigDecimal, K_MDRDMK_LDM_KORREKTUR: BigDecimal, S_LDM_SCHUB_80_PCNT: String, S_LDM_SCHUB_O_VERZ: String, S_SPT73_CP392994: String, Md_rad_soll_dcc: OutMeasurement, Md_rad_soll_sld: OutMeasurement, St_IfMgr_DrvrAsscSys: OutMeasurement, St_IfMgr_DrvrAsscSys_B_pmalq_akt: OutMeasurement, St_IfMgr_DrvrAsscSys_B_pmalq_anf: OutMeasurement, St_mdrdmk: OutMeasurement, St_mdrdmk_B_fas_dcc: OutMeasurement, St_mdrdmk_B_fas_sld: OutMeasurement, St_mdssm: OutMeasurement, St_mdssm_B_schalt_ldm: OutMeasurement): Unit = {
 ???
}

def P_MAFAS_10ms(a2lBin: A2LBinAdapter): Unit = {

val KL_MAFAS_LDM_BREMS: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MAFAS_LDM_BREMS")
val K_MAFAS_KEIN_KS: BigDecimal = a2lBin.readCharacteristicWithCast("K_MAFAS_KEIN_KS")
val K_MAFAS_MD_NAHE_DTORQ_BOT: BigDecimal = a2lBin.readCharacteristicWithCast("K_MAFAS_MD_NAHE_DTORQ_BOT")
val K_MAFAS_RAMP_DCC_OFF: BigDecimal = a2lBin.readCharacteristicWithCast("K_MAFAS_RAMP_DCC_OFF")
val K_MAFAS_RAMP_SLD_ERR: BigDecimal = a2lBin.readCharacteristicWithCast("K_MAFAS_RAMP_SLD_ERR")
val K_MAFAS_RAMP_SLD_OFF: BigDecimal = a2lBin.readCharacteristicWithCast("K_MAFAS_RAMP_SLD_OFF")
val K_MDRDMK_LDM_KORREKTUR: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDRDMK_LDM_KORREKTUR")
val S_LDM_SCHUB_80_PCNT: String = a2lBin.readCharacteristicWithCast("S_LDM_SCHUB_80_PCNT")
val S_LDM_SCHUB_O_VERZ: String = a2lBin.readCharacteristicWithCast("S_LDM_SCHUB_O_VERZ")
val S_SPT73_CP392994: String = a2lBin.readCharacteristicWithCast("S_SPT73_CP392994")
val BMWbdy_b_CluOp90_bo: InMeasurement = a2lBin.measurement("BMWbdy_b_CluOp90_bo")
val Md_rad_min_zka: InMeasurement = a2lBin.measurement("Md_rad_min_zka")
val Md_rad_schlepp: InMeasurement = a2lBin.measurement("Md_rad_schlepp")
val Md_rad_schlepp_soll: InMeasurement = a2lBin.measurement("Md_rad_schlepp_soll")
val Md_rad_soll: InMeasurement = a2lBin.measurement("Md_rad_soll")
val Md_rad_wunsch: InMeasurement = a2lBin.measurement("Md_rad_wunsch")
val St_anman1: InMeasurement = a2lBin.measurement("St_anman1")
val St_anman1_B_ldm_ena: InMeasurement = a2lBin.measurement("St_anman1.B_ldm_ena")
val St_anman1_B_nosa: InMeasurement = a2lBin.measurement("St_anman1.B_nosa")
val St_fas_mradsoll: InMeasurement = a2lBin.measurement("St_fas_mradsoll")
val St_ldm_kupp: InMeasurement = a2lBin.measurement("St_ldm_kupp")
val St_mdipmfw: InMeasurement = a2lBin.measurement("St_mdipmfw")
val St_mdipmfw_B_bst: InMeasurement = a2lBin.measurement("St_mdipmfw.B_bst")
val St_mdipmfw_B_emf_aktiv: InMeasurement = a2lBin.measurement("St_mdipmfw.B_emf_aktiv")
val St_mdipmfw_B_reku: InMeasurement = a2lBin.measurement("St_mdipmfw.B_reku")
val St_mdipmfw_B_schlepp: InMeasurement = a2lBin.measurement("St_mdipmfw.B_schlepp")
val Var_hs: InMeasurement = a2lBin.measurement("Var_hs")
val Md_rad_soll_dcc: OutMeasurement = a2lBin.measurement("Md_rad_soll_dcc")
val Md_rad_soll_sld: OutMeasurement = a2lBin.measurement("Md_rad_soll_sld")
val St_IfMgr_DrvrAsscSys: OutMeasurement = a2lBin.measurement("St_IfMgr_DrvrAsscSys")
val St_IfMgr_DrvrAsscSys_B_pmalq_akt: OutMeasurement = a2lBin.measurement("St_IfMgr_DrvrAsscSys.B_pmalq_akt")
val St_IfMgr_DrvrAsscSys_B_pmalq_anf: OutMeasurement = a2lBin.measurement("St_IfMgr_DrvrAsscSys.B_pmalq_anf")
val St_mdrdmk: OutMeasurement = a2lBin.measurement("St_mdrdmk")
val St_mdrdmk_B_fas_dcc: OutMeasurement = a2lBin.measurement("St_mdrdmk.B_fas_dcc")
val St_mdrdmk_B_fas_sld: OutMeasurement = a2lBin.measurement("St_mdrdmk.B_fas_sld")
val St_mdssm: OutMeasurement = a2lBin.measurement("St_mdssm")
val St_mdssm_B_schalt_ldm: OutMeasurement = a2lBin.measurement("St_mdssm.B_schalt_ldm")

  P_MAFAS_10ms(BMWbdy_b_CluOp90_bo, Md_rad_min_zka, Md_rad_schlepp, Md_rad_schlepp_soll, Md_rad_soll, Md_rad_wunsch, St_anman1, St_anman1_B_ldm_ena, St_anman1_B_nosa, St_fas_mradsoll, St_ldm_kupp, St_mdipmfw, St_mdipmfw_B_bst, St_mdipmfw_B_emf_aktiv, St_mdipmfw_B_reku, St_mdipmfw_B_schlepp, Var_hs, KL_MAFAS_LDM_BREMS, K_MAFAS_KEIN_KS, K_MAFAS_MD_NAHE_DTORQ_BOT, K_MAFAS_RAMP_DCC_OFF, K_MAFAS_RAMP_SLD_ERR, K_MAFAS_RAMP_SLD_OFF, K_MDRDMK_LDM_KORREKTUR, S_LDM_SCHUB_80_PCNT, S_LDM_SCHUB_O_VERZ, S_SPT73_CP392994, Md_rad_soll_dcc, Md_rad_soll_sld, St_IfMgr_DrvrAsscSys, St_IfMgr_DrvrAsscSys_B_pmalq_akt, St_IfMgr_DrvrAsscSys_B_pmalq_anf, St_mdrdmk, St_mdrdmk_B_fas_dcc, St_mdrdmk_B_fas_sld, St_mdssm, St_mdssm_B_schalt_ldm)
}


def P_MASSM_ANTRIEB_10ms(BMWSys_agGc_geMtSta_ub: InMeasurement, BMWbdy_b_CluOp10_bo: InMeasurement, BMWbdy_b_CluOp90_bo: InMeasurement, BMWmsa_stb_MsaDrr_ub: InMeasurement, BMWmsa_stb_MsaDrr_ub_B_fahrbereit: InMeasurement, BMWmsa_stb_MsaDrr_ub_B_fahrbereit_pwf: InMeasurement, BMWtqc_Rat_GbxWhlEaxl: InMeasurement, DispGrGrb: InMeasurement, DispGrPt_spa: InMeasurement, Fahrstufe_ist: InMeasurement, Fahrstufe_soll: InMeasurement, Gang_syn_engaged: InMeasurement, Gangi: InMeasurement, I_eff_gs_plaus: InMeasurement, Mdk_ist: InMeasurement, St_gang: InMeasurement, St_getrdaten: InMeasurement, St_getrdaten_B_gangwechsel_gs: InMeasurement, St_ngang0: InMeasurement, St_ngang0_B_gangnull: InMeasurement, St_ngang0_B_gangnullerw: InMeasurement, St_ngang0_B_ngangdok: InMeasurement, St_ngang0_B_nggelernt: InMeasurement, St_ngang0_B_ngimlf: InMeasurement, St_ngang0_B_nglernakt: InMeasurement, St_wk_plaus: InMeasurement, Status_antrieb_ist: InMeasurement, Status_iae_plaus: InMeasurement, Status_k0_plaus: InMeasurement, Status_ks_gb1_ist_plaus: InMeasurement, Status_ks_gb2_ist_plaus: InMeasurement, Status_mdred_egs_plaus: InMeasurement, Var_hs: InMeasurement, BMWtqw_gear_DispGear2DebGear_T: CurveType[BigDecimal, BigDecimal], CW_B_KS_EAXL_ANTRIEB: BigDecimal, CW_B_KS_HAXL_ANTRIEB_AT: BigDecimal, CW_B_KS_HAXL_ANTRIEB_HS: BigDecimal, CW_MASSM_01: BigDecimal, CW_ST_K0_KS: BigDecimal, CW_ST_WK_KS: BigDecimal, KL_KS_GB1_IST_ANTRIEB: CurveType[BigDecimal, BigDecimal], KL_KS_GB2_IST_ANTRIEB: CurveType[BigDecimal, BigDecimal], K_IEFFGS_KFS: BigDecimal, K_IEFFGS_KKS: BigDecimal, K_SCHMDKKS_SCH: BigDecimal, K_SCHMDKKS_ZUG: BigDecimal, K_ST_GANG_MASSM: BigDecimal, K_TDKEIN_GANG: BigDecimal, K_TD_KUPPEXT: BigDecimal, K_TD_MDKKS: BigDecimal, K_T_ENTPRGANG: BigDecimal, ST_LDM_KUPP_BN2010_C: String, ST_LDM_KUPP_BN2010_V: BigDecimal, S_CP456554_LDM: String, S_GANGWECHSEL_AN: BigDecimal, S_TEST_SET_B_ANTRIEB: String, BMWtqw_cw_FocDtExt_C: BigDecimal, BMWtqw_mask_RqrtGbxIntvFocDet_C: BigDecimal, BMWtqw_ti_RqrtGbxIntvFocDet_C: BigDecimal, CW_FREEZE_MDRAD_IAE: BigDecimal, Gangi_entpr: OutMeasurement, St_anman: OutMeasurement, St_anman_B_antrieb: OutMeasurement, St_anman_B_getreten: OutMeasurement, St_anman_B_kein_gang: OutMeasurement, St_anman_B_kupp_int: OutMeasurement, St_gang_massm: OutMeasurement, St_ldm_kupp: OutMeasurement, St_ldm_kupp_bn2010: OutMeasurement): Unit = {
 ???
}

def P_MASSM_ANTRIEB_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtqw_gear_DispGear2DebGear_T: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("BMWtqw_gear_DispGear2DebGear_T")
val CW_B_KS_EAXL_ANTRIEB: BigDecimal = a2lBin.readCharacteristicWithCast("CW_B_KS_EAXL_ANTRIEB")
val CW_B_KS_HAXL_ANTRIEB_AT: BigDecimal = a2lBin.readCharacteristicWithCast("CW_B_KS_HAXL_ANTRIEB_AT")
val CW_B_KS_HAXL_ANTRIEB_HS: BigDecimal = a2lBin.readCharacteristicWithCast("CW_B_KS_HAXL_ANTRIEB_HS")
val CW_MASSM_01: BigDecimal = a2lBin.readCharacteristicWithCast("CW_MASSM_01")
val CW_ST_K0_KS: BigDecimal = a2lBin.readCharacteristicWithCast("CW_ST_K0_KS")
val CW_ST_WK_KS: BigDecimal = a2lBin.readCharacteristicWithCast("CW_ST_WK_KS")
val KL_KS_GB1_IST_ANTRIEB: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_KS_GB1_IST_ANTRIEB")
val KL_KS_GB2_IST_ANTRIEB: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_KS_GB2_IST_ANTRIEB")
val K_IEFFGS_KFS: BigDecimal = a2lBin.readCharacteristicWithCast("K_IEFFGS_KFS")
val K_IEFFGS_KKS: BigDecimal = a2lBin.readCharacteristicWithCast("K_IEFFGS_KKS")
val K_SCHMDKKS_SCH: BigDecimal = a2lBin.readCharacteristicWithCast("K_SCHMDKKS_SCH")
val K_SCHMDKKS_ZUG: BigDecimal = a2lBin.readCharacteristicWithCast("K_SCHMDKKS_ZUG")
val K_ST_GANG_MASSM: BigDecimal = a2lBin.readCharacteristicWithCast("K_ST_GANG_MASSM")
val K_TDKEIN_GANG: BigDecimal = a2lBin.readCharacteristicWithCast("K_TDKEIN_GANG")
val K_TD_KUPPEXT: BigDecimal = a2lBin.readCharacteristicWithCast("K_TD_KUPPEXT")
val K_TD_MDKKS: BigDecimal = a2lBin.readCharacteristicWithCast("K_TD_MDKKS")
val K_T_ENTPRGANG: BigDecimal = a2lBin.readCharacteristicWithCast("K_T_ENTPRGANG")
val ST_LDM_KUPP_BN2010_C: String = a2lBin.readCharacteristicWithCast("ST_LDM_KUPP_BN2010_C")
val ST_LDM_KUPP_BN2010_V: BigDecimal = a2lBin.readCharacteristicWithCast("ST_LDM_KUPP_BN2010_V")
val S_CP456554_LDM: String = a2lBin.readCharacteristicWithCast("S_CP456554_LDM")
val S_GANGWECHSEL_AN: BigDecimal = a2lBin.readCharacteristicWithCast("S_GANGWECHSEL_AN")
val S_TEST_SET_B_ANTRIEB: String = a2lBin.readCharacteristicWithCast("S_TEST_SET_B_ANTRIEB")
val BMWtqw_cw_FocDtExt_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_cw_FocDtExt_C")
val BMWtqw_mask_RqrtGbxIntvFocDet_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_mask_RqrtGbxIntvFocDet_C")
val BMWtqw_ti_RqrtGbxIntvFocDet_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_ti_RqrtGbxIntvFocDet_C")
val CW_FREEZE_MDRAD_IAE: BigDecimal = a2lBin.readCharacteristicWithCast("CW_FREEZE_MDRAD_IAE")
val BMWSys_agGc_geMtSta_ub: InMeasurement = a2lBin.measurement("BMWSys_agGc_geMtSta_ub")
val BMWbdy_b_CluOp10_bo: InMeasurement = a2lBin.measurement("BMWbdy_b_CluOp10_bo")
val BMWbdy_b_CluOp90_bo: InMeasurement = a2lBin.measurement("BMWbdy_b_CluOp90_bo")
val BMWmsa_stb_MsaDrr_ub: InMeasurement = a2lBin.measurement("BMWmsa_stb_MsaDrr_ub")
val BMWmsa_stb_MsaDrr_ub_B_fahrbereit: InMeasurement = a2lBin.measurement("BMWmsa_stb_MsaDrr_ub.B_fahrbereit")
val BMWmsa_stb_MsaDrr_ub_B_fahrbereit_pwf: InMeasurement = a2lBin.measurement("BMWmsa_stb_MsaDrr_ub.B_fahrbereit_pwf")
val BMWtqc_Rat_GbxWhlEaxl: InMeasurement = a2lBin.measurement("BMWtqc_Rat_GbxWhlEaxl")
val DispGrGrb: InMeasurement = a2lBin.measurement("DispGrGrb")
val DispGrPt_spa: InMeasurement = a2lBin.measurement("DispGrPt_spa")
val Fahrstufe_ist: InMeasurement = a2lBin.measurement("Fahrstufe_ist")
val Fahrstufe_soll: InMeasurement = a2lBin.measurement("Fahrstufe_soll")
val Gang_syn_engaged: InMeasurement = a2lBin.measurement("Gang_syn_engaged")
val Gangi: InMeasurement = a2lBin.measurement("Gangi")
val I_eff_gs_plaus: InMeasurement = a2lBin.measurement("I_eff_gs_plaus")
val Mdk_ist: InMeasurement = a2lBin.measurement("Mdk_ist")
val St_gang: InMeasurement = a2lBin.measurement("St_gang")
val St_getrdaten: InMeasurement = a2lBin.measurement("St_getrdaten")
val St_getrdaten_B_gangwechsel_gs: InMeasurement = a2lBin.measurement("St_getrdaten.B_gangwechsel_gs")
val St_ngang0: InMeasurement = a2lBin.measurement("St_ngang0")
val St_ngang0_B_gangnull: InMeasurement = a2lBin.measurement("St_ngang0.B_gangnull")
val St_ngang0_B_gangnullerw: InMeasurement = a2lBin.measurement("St_ngang0.B_gangnullerw")
val St_ngang0_B_ngangdok: InMeasurement = a2lBin.measurement("St_ngang0.B_ngangdok")
val St_ngang0_B_nggelernt: InMeasurement = a2lBin.measurement("St_ngang0.B_nggelernt")
val St_ngang0_B_ngimlf: InMeasurement = a2lBin.measurement("St_ngang0.B_ngimlf")
val St_ngang0_B_nglernakt: InMeasurement = a2lBin.measurement("St_ngang0.B_nglernakt")
val St_wk_plaus: InMeasurement = a2lBin.measurement("St_wk_plaus")
val Status_antrieb_ist: InMeasurement = a2lBin.measurement("Status_antrieb_ist")
val Status_iae_plaus: InMeasurement = a2lBin.measurement("Status_iae_plaus")
val Status_k0_plaus: InMeasurement = a2lBin.measurement("Status_k0_plaus")
val Status_ks_gb1_ist_plaus: InMeasurement = a2lBin.measurement("Status_ks_gb1_ist_plaus")
val Status_ks_gb2_ist_plaus: InMeasurement = a2lBin.measurement("Status_ks_gb2_ist_plaus")
val Status_mdred_egs_plaus: InMeasurement = a2lBin.measurement("Status_mdred_egs_plaus")
val Var_hs: InMeasurement = a2lBin.measurement("Var_hs")
val Gangi_entpr: OutMeasurement = a2lBin.measurement("Gangi_entpr")
val St_anman: OutMeasurement = a2lBin.measurement("St_anman")
val St_anman_B_antrieb: OutMeasurement = a2lBin.measurement("St_anman.B_antrieb")
val St_anman_B_getreten: OutMeasurement = a2lBin.measurement("St_anman.B_getreten")
val St_anman_B_kein_gang: OutMeasurement = a2lBin.measurement("St_anman.B_kein_gang")
val St_anman_B_kupp_int: OutMeasurement = a2lBin.measurement("St_anman.B_kupp_int")
val St_gang_massm: OutMeasurement = a2lBin.measurement("St_gang_massm")
val St_ldm_kupp: OutMeasurement = a2lBin.measurement("St_ldm_kupp")
val St_ldm_kupp_bn2010: OutMeasurement = a2lBin.measurement("St_ldm_kupp_bn2010")

  P_MASSM_ANTRIEB_10ms(BMWSys_agGc_geMtSta_ub, BMWbdy_b_CluOp10_bo, BMWbdy_b_CluOp90_bo, BMWmsa_stb_MsaDrr_ub, BMWmsa_stb_MsaDrr_ub_B_fahrbereit, BMWmsa_stb_MsaDrr_ub_B_fahrbereit_pwf, BMWtqc_Rat_GbxWhlEaxl, DispGrGrb, DispGrPt_spa, Fahrstufe_ist, Fahrstufe_soll, Gang_syn_engaged, Gangi, I_eff_gs_plaus, Mdk_ist, St_gang, St_getrdaten, St_getrdaten_B_gangwechsel_gs, St_ngang0, St_ngang0_B_gangnull, St_ngang0_B_gangnullerw, St_ngang0_B_ngangdok, St_ngang0_B_nggelernt, St_ngang0_B_ngimlf, St_ngang0_B_nglernakt, St_wk_plaus, Status_antrieb_ist, Status_iae_plaus, Status_k0_plaus, Status_ks_gb1_ist_plaus, Status_ks_gb2_ist_plaus, Status_mdred_egs_plaus, Var_hs, BMWtqw_gear_DispGear2DebGear_T, CW_B_KS_EAXL_ANTRIEB, CW_B_KS_HAXL_ANTRIEB_AT, CW_B_KS_HAXL_ANTRIEB_HS, CW_MASSM_01, CW_ST_K0_KS, CW_ST_WK_KS, KL_KS_GB1_IST_ANTRIEB, KL_KS_GB2_IST_ANTRIEB, K_IEFFGS_KFS, K_IEFFGS_KKS, K_SCHMDKKS_SCH, K_SCHMDKKS_ZUG, K_ST_GANG_MASSM, K_TDKEIN_GANG, K_TD_KUPPEXT, K_TD_MDKKS, K_T_ENTPRGANG, ST_LDM_KUPP_BN2010_C, ST_LDM_KUPP_BN2010_V, S_CP456554_LDM, S_GANGWECHSEL_AN, S_TEST_SET_B_ANTRIEB, BMWtqw_cw_FocDtExt_C, BMWtqw_mask_RqrtGbxIntvFocDet_C, BMWtqw_ti_RqrtGbxIntvFocDet_C, CW_FREEZE_MDRAD_IAE, Gangi_entpr, St_anman, St_anman_B_antrieb, St_anman_B_getreten, St_anman_B_kein_gang, St_anman_B_kupp_int, St_gang_massm, St_ldm_kupp, St_ldm_kupp_bn2010)
}


def P_MASSM_BED_MDRAD_10ms(BMWbdy_b_CluOp90_bo: InMeasurement, St_anman: InMeasurement, St_anman_B_antrieb: InMeasurement, St_anman_B_getreten: InMeasurement, St_anman_B_kein_gang: InMeasurement, St_anman_B_kupp_int: InMeasurement, St_msae: InMeasurement, St_msae_B_fagurt: InMeasurement, St_msae_B_ftauf1: InMeasurement, St_msae_B_hkauf: InMeasurement, St_msae_B_mhauf1: InMeasurement, St_msae_B_msa_av_zl: InMeasurement, St_msae_B_msa_deakt_zl: InMeasurement, St_msae_B_msa_ea_zl: InMeasurement, St_msae_B_msafzg: InMeasurement, St_msae_B_msahistrst: InMeasurement, St_msae_B_msaobdav: InMeasurement, St_msae_B_msasw: InMeasurement, St_msae_B_msavadapt: InMeasurement, St_msae_B_nglern: InMeasurement, St_msae_B_schlok: InMeasurement, St_msae_B_zmsoff: InMeasurement, St_ngang0: InMeasurement, St_ngang0_B_gangnull: InMeasurement, St_ngang0_B_gangnullerw: InMeasurement, St_ngang0_B_ngangdok: InMeasurement, St_ngang0_B_nggelernt: InMeasurement, St_ngang0_B_ngimlf: InMeasurement, St_ngang0_B_nglernakt: InMeasurement, Status_iae_plaus: InMeasurement, Tvngang: InMeasurement, Var_hs: InMeasurement, CW_FREEZE_MDRAD_IAE: BigDecimal, CW_USE_MDRAD: BigDecimal, K_HYS_RMS_GANG: BigDecimal, K_SCHW_GANG_GERADE: BigDecimal, K_SCHW_GANG_UNGERADE: BigDecimal, BMWtqw_b_CdnActTqWhl_bo: OutMeasurement, St_rms_gang: OutMeasurement, St_rms_gang_B_rms_gang_gerade: OutMeasurement, St_rms_gang_B_rms_gang_null: OutMeasurement, St_rms_gang_B_rms_gang_ungerade: OutMeasurement): Unit = {
 ???
}

def P_MASSM_BED_MDRAD_10ms(a2lBin: A2LBinAdapter): Unit = {

val CW_FREEZE_MDRAD_IAE: BigDecimal = a2lBin.readCharacteristicWithCast("CW_FREEZE_MDRAD_IAE")
val CW_USE_MDRAD: BigDecimal = a2lBin.readCharacteristicWithCast("CW_USE_MDRAD")
val K_HYS_RMS_GANG: BigDecimal = a2lBin.readCharacteristicWithCast("K_HYS_RMS_GANG")
val K_SCHW_GANG_GERADE: BigDecimal = a2lBin.readCharacteristicWithCast("K_SCHW_GANG_GERADE")
val K_SCHW_GANG_UNGERADE: BigDecimal = a2lBin.readCharacteristicWithCast("K_SCHW_GANG_UNGERADE")
val BMWbdy_b_CluOp90_bo: InMeasurement = a2lBin.measurement("BMWbdy_b_CluOp90_bo")
val St_anman: InMeasurement = a2lBin.measurement("St_anman")
val St_anman_B_antrieb: InMeasurement = a2lBin.measurement("St_anman.B_antrieb")
val St_anman_B_getreten: InMeasurement = a2lBin.measurement("St_anman.B_getreten")
val St_anman_B_kein_gang: InMeasurement = a2lBin.measurement("St_anman.B_kein_gang")
val St_anman_B_kupp_int: InMeasurement = a2lBin.measurement("St_anman.B_kupp_int")
val St_msae: InMeasurement = a2lBin.measurement("St_msae")
val St_msae_B_fagurt: InMeasurement = a2lBin.measurement("St_msae.B_fagurt")
val St_msae_B_ftauf1: InMeasurement = a2lBin.measurement("St_msae.B_ftauf1")
val St_msae_B_hkauf: InMeasurement = a2lBin.measurement("St_msae.B_hkauf")
val St_msae_B_mhauf1: InMeasurement = a2lBin.measurement("St_msae.B_mhauf1")
val St_msae_B_msa_av_zl: InMeasurement = a2lBin.measurement("St_msae.B_msa_av_zl")
val St_msae_B_msa_deakt_zl: InMeasurement = a2lBin.measurement("St_msae.B_msa_deakt_zl")
val St_msae_B_msa_ea_zl: InMeasurement = a2lBin.measurement("St_msae.B_msa_ea_zl")
val St_msae_B_msafzg: InMeasurement = a2lBin.measurement("St_msae.B_msafzg")
val St_msae_B_msahistrst: InMeasurement = a2lBin.measurement("St_msae.B_msahistrst")
val St_msae_B_msaobdav: InMeasurement = a2lBin.measurement("St_msae.B_msaobdav")
val St_msae_B_msasw: InMeasurement = a2lBin.measurement("St_msae.B_msasw")
val St_msae_B_msavadapt: InMeasurement = a2lBin.measurement("St_msae.B_msavadapt")
val St_msae_B_nglern: InMeasurement = a2lBin.measurement("St_msae.B_nglern")
val St_msae_B_schlok: InMeasurement = a2lBin.measurement("St_msae.B_schlok")
val St_msae_B_zmsoff: InMeasurement = a2lBin.measurement("St_msae.B_zmsoff")
val St_ngang0: InMeasurement = a2lBin.measurement("St_ngang0")
val St_ngang0_B_gangnull: InMeasurement = a2lBin.measurement("St_ngang0.B_gangnull")
val St_ngang0_B_gangnullerw: InMeasurement = a2lBin.measurement("St_ngang0.B_gangnullerw")
val St_ngang0_B_ngangdok: InMeasurement = a2lBin.measurement("St_ngang0.B_ngangdok")
val St_ngang0_B_nggelernt: InMeasurement = a2lBin.measurement("St_ngang0.B_nggelernt")
val St_ngang0_B_ngimlf: InMeasurement = a2lBin.measurement("St_ngang0.B_ngimlf")
val St_ngang0_B_nglernakt: InMeasurement = a2lBin.measurement("St_ngang0.B_nglernakt")
val Status_iae_plaus: InMeasurement = a2lBin.measurement("Status_iae_plaus")
val Tvngang: InMeasurement = a2lBin.measurement("Tvngang")
val Var_hs: InMeasurement = a2lBin.measurement("Var_hs")
val BMWtqw_b_CdnActTqWhl_bo: OutMeasurement = a2lBin.measurement("BMWtqw_b_CdnActTqWhl_bo")
val St_rms_gang: OutMeasurement = a2lBin.measurement("St_rms_gang")
val St_rms_gang_B_rms_gang_gerade: OutMeasurement = a2lBin.measurement("St_rms_gang.B_rms_gang_gerade")
val St_rms_gang_B_rms_gang_null: OutMeasurement = a2lBin.measurement("St_rms_gang.B_rms_gang_null")
val St_rms_gang_B_rms_gang_ungerade: OutMeasurement = a2lBin.measurement("St_rms_gang.B_rms_gang_ungerade")

  P_MASSM_BED_MDRAD_10ms(BMWbdy_b_CluOp90_bo, St_anman, St_anman_B_antrieb, St_anman_B_getreten, St_anman_B_kein_gang, St_anman_B_kupp_int, St_msae, St_msae_B_fagurt, St_msae_B_ftauf1, St_msae_B_hkauf, St_msae_B_mhauf1, St_msae_B_msa_av_zl, St_msae_B_msa_deakt_zl, St_msae_B_msa_ea_zl, St_msae_B_msafzg, St_msae_B_msahistrst, St_msae_B_msaobdav, St_msae_B_msasw, St_msae_B_msavadapt, St_msae_B_nglern, St_msae_B_schlok, St_msae_B_zmsoff, St_ngang0, St_ngang0_B_gangnull, St_ngang0_B_gangnullerw, St_ngang0_B_ngangdok, St_ngang0_B_nggelernt, St_ngang0_B_ngimlf, St_ngang0_B_nglernakt, Status_iae_plaus, Tvngang, Var_hs, CW_FREEZE_MDRAD_IAE, CW_USE_MDRAD, K_HYS_RMS_GANG, K_SCHW_GANG_GERADE, K_SCHW_GANG_UNGERADE, BMWtqw_b_CdnActTqWhl_bo, St_rms_gang, St_rms_gang_B_rms_gang_gerade, St_rms_gang_B_rms_gang_null, St_rms_gang_B_rms_gang_ungerade)
}


def P_MASSM_MDINFO_10ms(St_fw: InMeasurement, St_fw_B_hschalt_komb: InMeasurement, St_fw_B_ldm_akt04: InMeasurement, St_fw_B_ldm_off: InMeasurement, St_fw_B_ldm_offhs: InMeasurement, St_fw_B_ldm_offreg: InMeasurement, St_fw_B_ldm_offrs: InMeasurement, St_fw_B_rschalt_komb: InMeasurement, St_mdfw: InMeasurement, St_mdfw_B_sld_akt: InMeasurement, St_mdinfo_madmk: InMeasurement, St_mdinfo_madmk_B_emf_lim: InMeasurement, St_mdinfo_madmk_B_steigbegr_lim: InMeasurement, St_mdinfo_madmk_B_vlim_lim: InMeasurement, St_mdinfo_maw: InMeasurement, St_mdinfo_maw_B_fd_max_akt: InMeasurement, St_mdinfo_maw_B_fd_min_akt: InMeasurement, St_mxxlim_1: InMeasurement, St_mxxlim_1_B_asr_mxxlim: InMeasurement, St_mxxlim_1_B_msr_mxxlim: InMeasurement, CW_MDINFO_SCHALTER: BigDecimal, CW_MDINFO_WERT: BigDecimal, Stat_mdinfo_s_tqw: OutMeasurement, Stat_mdinfo_tqw: OutMeasurement): Unit = {
 ???
}

def P_MASSM_MDINFO_10ms(a2lBin: A2LBinAdapter): Unit = {

val CW_MDINFO_SCHALTER: BigDecimal = a2lBin.readCharacteristicWithCast("CW_MDINFO_SCHALTER")
val CW_MDINFO_WERT: BigDecimal = a2lBin.readCharacteristicWithCast("CW_MDINFO_WERT")
val St_fw: InMeasurement = a2lBin.measurement("St_fw")
val St_fw_B_hschalt_komb: InMeasurement = a2lBin.measurement("St_fw.B_hschalt_komb")
val St_fw_B_ldm_akt04: InMeasurement = a2lBin.measurement("St_fw.B_ldm_akt04")
val St_fw_B_ldm_off: InMeasurement = a2lBin.measurement("St_fw.B_ldm_off")
val St_fw_B_ldm_offhs: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offhs")
val St_fw_B_ldm_offreg: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offreg")
val St_fw_B_ldm_offrs: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offrs")
val St_fw_B_rschalt_komb: InMeasurement = a2lBin.measurement("St_fw.B_rschalt_komb")
val St_mdfw: InMeasurement = a2lBin.measurement("St_mdfw")
val St_mdfw_B_sld_akt: InMeasurement = a2lBin.measurement("St_mdfw.B_sld_akt")
val St_mdinfo_madmk: InMeasurement = a2lBin.measurement("St_mdinfo_madmk")
val St_mdinfo_madmk_B_emf_lim: InMeasurement = a2lBin.measurement("St_mdinfo_madmk.B_emf_lim")
val St_mdinfo_madmk_B_steigbegr_lim: InMeasurement = a2lBin.measurement("St_mdinfo_madmk.B_steigbegr_lim")
val St_mdinfo_madmk_B_vlim_lim: InMeasurement = a2lBin.measurement("St_mdinfo_madmk.B_vlim_lim")
val St_mdinfo_maw: InMeasurement = a2lBin.measurement("St_mdinfo_maw")
val St_mdinfo_maw_B_fd_max_akt: InMeasurement = a2lBin.measurement("St_mdinfo_maw.B_fd_max_akt")
val St_mdinfo_maw_B_fd_min_akt: InMeasurement = a2lBin.measurement("St_mdinfo_maw.B_fd_min_akt")
val St_mxxlim_1: InMeasurement = a2lBin.measurement("St_mxxlim_1")
val St_mxxlim_1_B_asr_mxxlim: InMeasurement = a2lBin.measurement("St_mxxlim_1.B_asr_mxxlim")
val St_mxxlim_1_B_msr_mxxlim: InMeasurement = a2lBin.measurement("St_mxxlim_1.B_msr_mxxlim")
val Stat_mdinfo_s_tqw: OutMeasurement = a2lBin.measurement("Stat_mdinfo_s_tqw")
val Stat_mdinfo_tqw: OutMeasurement = a2lBin.measurement("Stat_mdinfo_tqw")

  P_MASSM_MDINFO_10ms(St_fw, St_fw_B_hschalt_komb, St_fw_B_ldm_akt04, St_fw_B_ldm_off, St_fw_B_ldm_offhs, St_fw_B_ldm_offreg, St_fw_B_ldm_offrs, St_fw_B_rschalt_komb, St_mdfw, St_mdfw_B_sld_akt, St_mdinfo_madmk, St_mdinfo_madmk_B_emf_lim, St_mdinfo_madmk_B_steigbegr_lim, St_mdinfo_madmk_B_vlim_lim, St_mdinfo_maw, St_mdinfo_maw_B_fd_max_akt, St_mdinfo_maw_B_fd_min_akt, St_mxxlim_1, St_mxxlim_1_B_asr_mxxlim, St_mxxlim_1_B_msr_mxxlim, CW_MDINFO_SCHALTER, CW_MDINFO_WERT, Stat_mdinfo_s_tqw, Stat_mdinfo_tqw)
}


def P_MASSM_NL(St_fhrt_rchtg: InMeasurement, St_gang: InMeasurement, St_ldm_kupp: InMeasurement, St_ldm_kupp_bn2010: InMeasurement, St_sq_rekup: InMeasurement, Var_hs: InMeasurement, K_ST_SQ_REKUP_NL_LK: BigDecimal): Unit = {
 ???
}

def P_MASSM_NL(a2lBin: A2LBinAdapter): Unit = {

val K_ST_SQ_REKUP_NL_LK: BigDecimal = a2lBin.readCharacteristicWithCast("K_ST_SQ_REKUP_NL_LK")
val St_fhrt_rchtg: InMeasurement = a2lBin.measurement("St_fhrt_rchtg")
val St_gang: InMeasurement = a2lBin.measurement("St_gang")
val St_ldm_kupp: InMeasurement = a2lBin.measurement("St_ldm_kupp")
val St_ldm_kupp_bn2010: InMeasurement = a2lBin.measurement("St_ldm_kupp_bn2010")
val St_sq_rekup: InMeasurement = a2lBin.measurement("St_sq_rekup")
val Var_hs: InMeasurement = a2lBin.measurement("Var_hs")


  P_MASSM_NL(St_fhrt_rchtg, St_gang, St_ldm_kupp, St_ldm_kupp_bn2010, St_sq_rekup, Var_hs, K_ST_SQ_REKUP_NL_LK)
}


def P_MASSM_SERVQUAL_10ms(BMWSys_agGc_geMtQly_ub: InMeasurement, BMWSys_agGc_geMtSta_ub: InMeasurement, BMWbdy_b_RvsGear_bo: InMeasurement, BMWmsa_stb_MsaDrr_ub: InMeasurement, BMWmsa_stb_MsaDrr_ub_B_fahrbereit: InMeasurement, BMWmsa_stb_MsaDrr_ub_B_fahrbereit_pwf: InMeasurement, Nkw_grad: InMeasurement, Pwg_ist: InMeasurement, St_MASSM_BEDMDRAD: InMeasurement, St_MASSM_BEDMDRAD_B_MASSM_Init: InMeasurement, St_anman: InMeasurement, St_anman_B_antrieb: InMeasurement, St_anman_B_getreten: InMeasurement, St_anman_B_kein_gang: InMeasurement, St_anman_B_kupp_int: InMeasurement, St_anman1: InMeasurement, St_anman1_B_ldm_ena: InMeasurement, St_anman1_B_nosa: InMeasurement, St_fw: InMeasurement, St_fw_B_hschalt_komb: InMeasurement, St_fw_B_ldm_akt04: InMeasurement, St_fw_B_ldm_off: InMeasurement, St_fw_B_ldm_offhs: InMeasurement, St_fw_B_ldm_offreg: InMeasurement, St_fw_B_ldm_offrs: InMeasurement, St_fw_B_rschalt_komb: InMeasurement, St_gang_massm: InMeasurement, St_mdfw: InMeasurement, St_mdfw_B_sld_akt: InMeasurement, St_mdrdmk: InMeasurement, St_mdrdmk_B_fas_dcc: InMeasurement, St_mdrdmk_B_fas_sld: InMeasurement, St_msae: InMeasurement, St_msae_B_fagurt: InMeasurement, St_msae_B_ftauf1: InMeasurement, St_msae_B_hkauf: InMeasurement, St_msae_B_mhauf1: InMeasurement, St_msae_B_msa_av_zl: InMeasurement, St_msae_B_msa_deakt_zl: InMeasurement, St_msae_B_msa_ea_zl: InMeasurement, St_msae_B_msafzg: InMeasurement, St_msae_B_msahistrst: InMeasurement, St_msae_B_msaobdav: InMeasurement, St_msae_B_msasw: InMeasurement, St_msae_B_msavadapt: InMeasurement, St_msae_B_nglern: InMeasurement, St_msae_B_schlok: InMeasurement, St_msae_B_zmsoff: InMeasurement, St_rms_gang: InMeasurement, St_rms_gang_B_rms_gang_gerade: InMeasurement, St_rms_gang_B_rms_gang_null: InMeasurement, St_rms_gang_B_rms_gang_ungerade: InMeasurement, Var_hs: InMeasurement, BMWtqw_tqw_VsbRqrtFidExtEna_C: BigDecimal, K_DREHT_STDSCMRAD: BigDecimal, K_DREHT_ST_RADIST: BigDecimal, K_FEHLER_STDSCMRAD: BigDecimal, K_NO_DREHT_STDSCMRAD: BigDecimal, K_NO_DREHT_ST_RADIST: BigDecimal, K_PWG_MIN: BigDecimal, K_SLD_GETR: BigDecimal, K_SLD_UEBER: BigDecimal, K_SLD_UNT: BigDecimal, K_ST_FHRT_RCHTG_FEHLER: BigDecimal, K_ZK_NKWGRAD: BigDecimal, ST_DSC_MRADIST_C: String, ST_DSC_MRADIST_V: BigDecimal, ST_FHRT_RCHTG_C: String, ST_FHRT_RCHTG_V: BigDecimal, ST_INFO_PEDFAS_C: String, ST_INFO_PEDFAS_V: BigDecimal, ST_RAD_IST_C: String, ST_RAD_IST_V: BigDecimal, S_CP456554_FHRT: String, S_CP456554_REVGEAR: String, S_GANGSENSOR_VORHANDEN: BigDecimal, Nkw_grad_fpt1: OutMeasurement, St_dsc_mradist: OutMeasurement, St_fhrt_rchtg: OutMeasurement, St_info_pedfas: OutMeasurement, St_rad_ist: OutMeasurement): Unit = {
 ???
}

def P_MASSM_SERVQUAL_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtqw_tqw_VsbRqrtFidExtEna_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_tqw_VsbRqrtFidExtEna_C")
val K_DREHT_STDSCMRAD: BigDecimal = a2lBin.readCharacteristicWithCast("K_DREHT_STDSCMRAD")
val K_DREHT_ST_RADIST: BigDecimal = a2lBin.readCharacteristicWithCast("K_DREHT_ST_RADIST")
val K_FEHLER_STDSCMRAD: BigDecimal = a2lBin.readCharacteristicWithCast("K_FEHLER_STDSCMRAD")
val K_NO_DREHT_STDSCMRAD: BigDecimal = a2lBin.readCharacteristicWithCast("K_NO_DREHT_STDSCMRAD")
val K_NO_DREHT_ST_RADIST: BigDecimal = a2lBin.readCharacteristicWithCast("K_NO_DREHT_ST_RADIST")
val K_PWG_MIN: BigDecimal = a2lBin.readCharacteristicWithCast("K_PWG_MIN")
val K_SLD_GETR: BigDecimal = a2lBin.readCharacteristicWithCast("K_SLD_GETR")
val K_SLD_UEBER: BigDecimal = a2lBin.readCharacteristicWithCast("K_SLD_UEBER")
val K_SLD_UNT: BigDecimal = a2lBin.readCharacteristicWithCast("K_SLD_UNT")
val K_ST_FHRT_RCHTG_FEHLER: BigDecimal = a2lBin.readCharacteristicWithCast("K_ST_FHRT_RCHTG_FEHLER")
val K_ZK_NKWGRAD: BigDecimal = a2lBin.readCharacteristicWithCast("K_ZK_NKWGRAD")
val ST_DSC_MRADIST_C: String = a2lBin.readCharacteristicWithCast("ST_DSC_MRADIST_C")
val ST_DSC_MRADIST_V: BigDecimal = a2lBin.readCharacteristicWithCast("ST_DSC_MRADIST_V")
val ST_FHRT_RCHTG_C: String = a2lBin.readCharacteristicWithCast("ST_FHRT_RCHTG_C")
val ST_FHRT_RCHTG_V: BigDecimal = a2lBin.readCharacteristicWithCast("ST_FHRT_RCHTG_V")
val ST_INFO_PEDFAS_C: String = a2lBin.readCharacteristicWithCast("ST_INFO_PEDFAS_C")
val ST_INFO_PEDFAS_V: BigDecimal = a2lBin.readCharacteristicWithCast("ST_INFO_PEDFAS_V")
val ST_RAD_IST_C: String = a2lBin.readCharacteristicWithCast("ST_RAD_IST_C")
val ST_RAD_IST_V: BigDecimal = a2lBin.readCharacteristicWithCast("ST_RAD_IST_V")
val S_CP456554_FHRT: String = a2lBin.readCharacteristicWithCast("S_CP456554_FHRT")
val S_CP456554_REVGEAR: String = a2lBin.readCharacteristicWithCast("S_CP456554_REVGEAR")
val S_GANGSENSOR_VORHANDEN: BigDecimal = a2lBin.readCharacteristicWithCast("S_GANGSENSOR_VORHANDEN")
val BMWSys_agGc_geMtQly_ub: InMeasurement = a2lBin.measurement("BMWSys_agGc_geMtQly_ub")
val BMWSys_agGc_geMtSta_ub: InMeasurement = a2lBin.measurement("BMWSys_agGc_geMtSta_ub")
val BMWbdy_b_RvsGear_bo: InMeasurement = a2lBin.measurement("BMWbdy_b_RvsGear_bo")
val BMWmsa_stb_MsaDrr_ub: InMeasurement = a2lBin.measurement("BMWmsa_stb_MsaDrr_ub")
val BMWmsa_stb_MsaDrr_ub_B_fahrbereit: InMeasurement = a2lBin.measurement("BMWmsa_stb_MsaDrr_ub.B_fahrbereit")
val BMWmsa_stb_MsaDrr_ub_B_fahrbereit_pwf: InMeasurement = a2lBin.measurement("BMWmsa_stb_MsaDrr_ub.B_fahrbereit_pwf")
val Nkw_grad: InMeasurement = a2lBin.measurement("Nkw_grad")
val Pwg_ist: InMeasurement = a2lBin.measurement("Pwg_ist")
val St_MASSM_BEDMDRAD: InMeasurement = a2lBin.measurement("St_MASSM_BEDMDRAD")
val St_MASSM_BEDMDRAD_B_MASSM_Init: InMeasurement = a2lBin.measurement("St_MASSM_BEDMDRAD.B_MASSM_Init")
val St_anman: InMeasurement = a2lBin.measurement("St_anman")
val St_anman_B_antrieb: InMeasurement = a2lBin.measurement("St_anman.B_antrieb")
val St_anman_B_getreten: InMeasurement = a2lBin.measurement("St_anman.B_getreten")
val St_anman_B_kein_gang: InMeasurement = a2lBin.measurement("St_anman.B_kein_gang")
val St_anman_B_kupp_int: InMeasurement = a2lBin.measurement("St_anman.B_kupp_int")
val St_anman1: InMeasurement = a2lBin.measurement("St_anman1")
val St_anman1_B_ldm_ena: InMeasurement = a2lBin.measurement("St_anman1.B_ldm_ena")
val St_anman1_B_nosa: InMeasurement = a2lBin.measurement("St_anman1.B_nosa")
val St_fw: InMeasurement = a2lBin.measurement("St_fw")
val St_fw_B_hschalt_komb: InMeasurement = a2lBin.measurement("St_fw.B_hschalt_komb")
val St_fw_B_ldm_akt04: InMeasurement = a2lBin.measurement("St_fw.B_ldm_akt04")
val St_fw_B_ldm_off: InMeasurement = a2lBin.measurement("St_fw.B_ldm_off")
val St_fw_B_ldm_offhs: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offhs")
val St_fw_B_ldm_offreg: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offreg")
val St_fw_B_ldm_offrs: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offrs")
val St_fw_B_rschalt_komb: InMeasurement = a2lBin.measurement("St_fw.B_rschalt_komb")
val St_gang_massm: InMeasurement = a2lBin.measurement("St_gang_massm")
val St_mdfw: InMeasurement = a2lBin.measurement("St_mdfw")
val St_mdfw_B_sld_akt: InMeasurement = a2lBin.measurement("St_mdfw.B_sld_akt")
val St_mdrdmk: InMeasurement = a2lBin.measurement("St_mdrdmk")
val St_mdrdmk_B_fas_dcc: InMeasurement = a2lBin.measurement("St_mdrdmk.B_fas_dcc")
val St_mdrdmk_B_fas_sld: InMeasurement = a2lBin.measurement("St_mdrdmk.B_fas_sld")
val St_msae: InMeasurement = a2lBin.measurement("St_msae")
val St_msae_B_fagurt: InMeasurement = a2lBin.measurement("St_msae.B_fagurt")
val St_msae_B_ftauf1: InMeasurement = a2lBin.measurement("St_msae.B_ftauf1")
val St_msae_B_hkauf: InMeasurement = a2lBin.measurement("St_msae.B_hkauf")
val St_msae_B_mhauf1: InMeasurement = a2lBin.measurement("St_msae.B_mhauf1")
val St_msae_B_msa_av_zl: InMeasurement = a2lBin.measurement("St_msae.B_msa_av_zl")
val St_msae_B_msa_deakt_zl: InMeasurement = a2lBin.measurement("St_msae.B_msa_deakt_zl")
val St_msae_B_msa_ea_zl: InMeasurement = a2lBin.measurement("St_msae.B_msa_ea_zl")
val St_msae_B_msafzg: InMeasurement = a2lBin.measurement("St_msae.B_msafzg")
val St_msae_B_msahistrst: InMeasurement = a2lBin.measurement("St_msae.B_msahistrst")
val St_msae_B_msaobdav: InMeasurement = a2lBin.measurement("St_msae.B_msaobdav")
val St_msae_B_msasw: InMeasurement = a2lBin.measurement("St_msae.B_msasw")
val St_msae_B_msavadapt: InMeasurement = a2lBin.measurement("St_msae.B_msavadapt")
val St_msae_B_nglern: InMeasurement = a2lBin.measurement("St_msae.B_nglern")
val St_msae_B_schlok: InMeasurement = a2lBin.measurement("St_msae.B_schlok")
val St_msae_B_zmsoff: InMeasurement = a2lBin.measurement("St_msae.B_zmsoff")
val St_rms_gang: InMeasurement = a2lBin.measurement("St_rms_gang")
val St_rms_gang_B_rms_gang_gerade: InMeasurement = a2lBin.measurement("St_rms_gang.B_rms_gang_gerade")
val St_rms_gang_B_rms_gang_null: InMeasurement = a2lBin.measurement("St_rms_gang.B_rms_gang_null")
val St_rms_gang_B_rms_gang_ungerade: InMeasurement = a2lBin.measurement("St_rms_gang.B_rms_gang_ungerade")
val Var_hs: InMeasurement = a2lBin.measurement("Var_hs")
val Nkw_grad_fpt1: OutMeasurement = a2lBin.measurement("Nkw_grad_fpt1")
val St_dsc_mradist: OutMeasurement = a2lBin.measurement("St_dsc_mradist")
val St_fhrt_rchtg: OutMeasurement = a2lBin.measurement("St_fhrt_rchtg")
val St_info_pedfas: OutMeasurement = a2lBin.measurement("St_info_pedfas")
val St_rad_ist: OutMeasurement = a2lBin.measurement("St_rad_ist")

  P_MASSM_SERVQUAL_10ms(BMWSys_agGc_geMtQly_ub, BMWSys_agGc_geMtSta_ub, BMWbdy_b_RvsGear_bo, BMWmsa_stb_MsaDrr_ub, BMWmsa_stb_MsaDrr_ub_B_fahrbereit, BMWmsa_stb_MsaDrr_ub_B_fahrbereit_pwf, Nkw_grad, Pwg_ist, St_MASSM_BEDMDRAD, St_MASSM_BEDMDRAD_B_MASSM_Init, St_anman, St_anman_B_antrieb, St_anman_B_getreten, St_anman_B_kein_gang, St_anman_B_kupp_int, St_anman1, St_anman1_B_ldm_ena, St_anman1_B_nosa, St_fw, St_fw_B_hschalt_komb, St_fw_B_ldm_akt04, St_fw_B_ldm_off, St_fw_B_ldm_offhs, St_fw_B_ldm_offreg, St_fw_B_ldm_offrs, St_fw_B_rschalt_komb, St_gang_massm, St_mdfw, St_mdfw_B_sld_akt, St_mdrdmk, St_mdrdmk_B_fas_dcc, St_mdrdmk_B_fas_sld, St_msae, St_msae_B_fagurt, St_msae_B_ftauf1, St_msae_B_hkauf, St_msae_B_mhauf1, St_msae_B_msa_av_zl, St_msae_B_msa_deakt_zl, St_msae_B_msa_ea_zl, St_msae_B_msafzg, St_msae_B_msahistrst, St_msae_B_msaobdav, St_msae_B_msasw, St_msae_B_msavadapt, St_msae_B_nglern, St_msae_B_schlok, St_msae_B_zmsoff, St_rms_gang, St_rms_gang_B_rms_gang_gerade, St_rms_gang_B_rms_gang_null, St_rms_gang_B_rms_gang_ungerade, Var_hs, BMWtqw_tqw_VsbRqrtFidExtEna_C, K_DREHT_STDSCMRAD, K_DREHT_ST_RADIST, K_FEHLER_STDSCMRAD, K_NO_DREHT_STDSCMRAD, K_NO_DREHT_ST_RADIST, K_PWG_MIN, K_SLD_GETR, K_SLD_UEBER, K_SLD_UNT, K_ST_FHRT_RCHTG_FEHLER, K_ZK_NKWGRAD, ST_DSC_MRADIST_C, ST_DSC_MRADIST_V, ST_FHRT_RCHTG_C, ST_FHRT_RCHTG_V, ST_INFO_PEDFAS_C, ST_INFO_PEDFAS_V, ST_RAD_IST_C, ST_RAD_IST_V, S_CP456554_FHRT, S_CP456554_REVGEAR, S_GANGSENSOR_VORHANDEN, Nkw_grad_fpt1, St_dsc_mradist, St_fhrt_rchtg, St_info_pedfas, St_rad_ist)
}


def P_MASSM_ini2(Gangi: InMeasurement, Gangi_entpr: InMeasurement, St_MASSM_BEDMDRAD: OutMeasurement, St_MASSM_BEDMDRAD_B_MASSM_Init: OutMeasurement): Unit = {
 ???
}

def P_MASSM_ini2(a2lBin: A2LBinAdapter): Unit = {


val Gangi: InMeasurement = a2lBin.measurement("Gangi")
val Gangi_entpr: InMeasurement = a2lBin.measurement("Gangi_entpr")
val St_MASSM_BEDMDRAD: OutMeasurement = a2lBin.measurement("St_MASSM_BEDMDRAD")
val St_MASSM_BEDMDRAD_B_MASSM_Init: OutMeasurement = a2lBin.measurement("St_MASSM_BEDMDRAD.B_MASSM_Init")

  P_MASSM_ini2(Gangi, Gangi_entpr, St_MASSM_BEDMDRAD, St_MASSM_BEDMDRAD_B_MASSM_Init)
}


def P_MASTAB_10ms(BMWvm_tq_TarTqWhlStabnFas_sw: InMeasurement, BMWvm_tq_TarTqWhlStabn_sw: InMeasurement, St_dsc_mradsoll: InMeasurement, CW_MASTAB_ENABLE_FID_PRI: BigDecimal, CW_MASTAB_ENABLE_FID_PRI_FAST: BigDecimal, CW_MASTAB_ENABLE_FID_SEC: BigDecimal, K_MD_RAD_STAB_MAX_PRI_AX_GRD: Array[BigDecimal], K_MD_RAD_STAB_MIN_PRI_AX_GRD: Array[BigDecimal], S_USE_KSINFO_4_ASR: String, S_USE_KSINFO_4_MSR: String, BMWtqw_tqw_VsbHaMaxDyn_sw: OutMeasurement, BMWtqw_tqw_VsbHaMaxStat_sw: OutMeasurement, BMWtqw_tqw_VsbHaMinDyn_sw: OutMeasurement, BMWtqw_tqw_VsbHaMinStat_sw: OutMeasurement): Unit = {
 ???
}

def P_MASTAB_10ms(a2lBin: A2LBinAdapter): Unit = {

val CW_MASTAB_ENABLE_FID_PRI: BigDecimal = a2lBin.readCharacteristicWithCast("CW_MASTAB_ENABLE_FID_PRI")
val CW_MASTAB_ENABLE_FID_PRI_FAST: BigDecimal = a2lBin.readCharacteristicWithCast("CW_MASTAB_ENABLE_FID_PRI_FAST")
val CW_MASTAB_ENABLE_FID_SEC: BigDecimal = a2lBin.readCharacteristicWithCast("CW_MASTAB_ENABLE_FID_SEC")
val K_MD_RAD_STAB_MAX_PRI_AX_GRD: Array[BigDecimal] = a2lBin.readCharacteristicWithCast("K_MD_RAD_STAB_MAX_PRI_AX_GRD")
val K_MD_RAD_STAB_MIN_PRI_AX_GRD: Array[BigDecimal] = a2lBin.readCharacteristicWithCast("K_MD_RAD_STAB_MIN_PRI_AX_GRD")
val S_USE_KSINFO_4_ASR: String = a2lBin.readCharacteristicWithCast("S_USE_KSINFO_4_ASR")
val S_USE_KSINFO_4_MSR: String = a2lBin.readCharacteristicWithCast("S_USE_KSINFO_4_MSR")
val BMWvm_tq_TarTqWhlStabnFas_sw: InMeasurement = a2lBin.measurement("BMWvm_tq_TarTqWhlStabnFas_sw")
val BMWvm_tq_TarTqWhlStabn_sw: InMeasurement = a2lBin.measurement("BMWvm_tq_TarTqWhlStabn_sw")
val St_dsc_mradsoll: InMeasurement = a2lBin.measurement("St_dsc_mradsoll")
val BMWtqw_tqw_VsbHaMaxDyn_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_VsbHaMaxDyn_sw")
val BMWtqw_tqw_VsbHaMaxStat_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_VsbHaMaxStat_sw")
val BMWtqw_tqw_VsbHaMinDyn_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_VsbHaMinDyn_sw")
val BMWtqw_tqw_VsbHaMinStat_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_VsbHaMinStat_sw")

  P_MASTAB_10ms(BMWvm_tq_TarTqWhlStabnFas_sw, BMWvm_tq_TarTqWhlStabn_sw, St_dsc_mradsoll, CW_MASTAB_ENABLE_FID_PRI, CW_MASTAB_ENABLE_FID_PRI_FAST, CW_MASTAB_ENABLE_FID_SEC, K_MD_RAD_STAB_MAX_PRI_AX_GRD, K_MD_RAD_STAB_MIN_PRI_AX_GRD, S_USE_KSINFO_4_ASR, S_USE_KSINFO_4_MSR, BMWtqw_tqw_VsbHaMaxDyn_sw, BMWtqw_tqw_VsbHaMaxStat_sw, BMWtqw_tqw_VsbHaMinDyn_sw, BMWtqw_tqw_VsbHaMinStat_sw)
}


def P_MAWF_DASH_10ms(A_quer_abs: InMeasurement, BMWbdy_b_CluOp10_bo: InMeasurement, BMWmsa_stb_MsaDrr_ub: InMeasurement, BMWmsa_stb_MsaDrr_ub_B_fahrbereit: InMeasurement, BMWmsa_stb_MsaDrr_ub_B_fahrbereit_pwf: InMeasurement, BMWtqe_st_tq_LimSrc_ul: InMeasurement, BMWtqw_tqw_AxcSumMaxDyn_sw: InMeasurement, BMWtqw_tqw_AxcSumMinDyn_sw: InMeasurement, BMWtqw_tqw_AxcSumMinStat_sw: InMeasurement, B_sport_in_2: InMeasurement, Gangi: InMeasurement, I_ges_vh: InMeasurement, Md_rad_brems_antr_soll: InMeasurement, Md_rad_fzdyn_int: InMeasurement, Md_rad_ini_dp: InMeasurement, Md_rad_lsd_dash: InMeasurement, Md_rad_max_zka: InMeasurement, Md_rad_min_zka: InMeasurement, Md_rad_pedal_ap: InMeasurement, Md_rad_schlepp_soll: InMeasurement, Md_rad_wunsch_begr: InMeasurement, Md_rad_wunsch_fas: InMeasurement, Md_rad_wunsch_grad: InMeasurement, Md_rad_wunsch_vb: InMeasurement, St_fw: InMeasurement, St_fw_B_hschalt_komb: InMeasurement, St_fw_B_ldm_akt04: InMeasurement, St_fw_B_ldm_off: InMeasurement, St_fw_B_ldm_offhs: InMeasurement, St_fw_B_ldm_offreg: InMeasurement, St_fw_B_ldm_offrs: InMeasurement, St_fw_B_rschalt_komb: InMeasurement, St_getrdaten: InMeasurement, St_getrdaten_B_gangwechsel_gs: InMeasurement, St_kupp_dkg_plaus: InMeasurement, St_mdar0: InMeasurement, St_mdar0_B_edp: InMeasurement, St_mdar0_B_elsd: InMeasurement, St_mdar0_B_lsd: InMeasurement, St_mdfw: InMeasurement, St_mdfw_B_sld_akt: InMeasurement, St_mdipmfw: InMeasurement, St_mdipmfw_B_bst: InMeasurement, St_mdipmfw_B_emf_aktiv: InMeasurement, St_mdipmfw_B_reku: InMeasurement, St_mdipmfw_B_schlepp: InMeasurement, St_mdldynf: InMeasurement, St_mdldynf_B_sport_in: InMeasurement, V_fzg_fahrtricht: InMeasurement, Var_at: InMeasurement, Var_dkg: InMeasurement, Zka_fak: InMeasurement, CW_CONF_DASH: BigDecimal, CW_STMDINFO_DASH: BigDecimal, CW_S_SPORT: BigDecimal, KF_MAWF_DMDP1: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_DMDP1A: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_DMDP2: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_DMDP2A: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_DMDP3: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_DMDP3A: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_DMDPOFF: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_DMDPON: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_FDMDP3_MN: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_F_SPORT_2_DASH3: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_MDR_DP_O: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_MDR_DP_OA: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_MDR_DP_U: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_MDR_DP_UA: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_ZKDP3: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_ZKDP3A: MapType[BigDecimal, BigDecimal, BigDecimal], KL_MAWF_FDMDP1_BREMS: CurveType[BigDecimal, BigDecimal], KL_MAWF_FDMDP2_BREMS: CurveType[BigDecimal, BigDecimal], KL_MAWF_FDMDP3_BREMS: CurveType[BigDecimal, BigDecimal], KL_MAWF_FDMDPA1: CurveType[BigDecimal, BigDecimal], KL_MAWF_FDMDPA2: CurveType[BigDecimal, BigDecimal], KL_MAWF_FDMDPA3: CurveType[BigDecimal, BigDecimal], KL_MAWF_F_SPORT_DASH1: CurveType[BigDecimal, BigDecimal], KL_MAWF_F_SPORT_DASH2: CurveType[BigDecimal, BigDecimal], KL_MAWF_F_SPORT_DASH3: CurveType[BigDecimal, BigDecimal], K_MAWF_AQMAX_SPS: BigDecimal, K_MAWF_DMDRDP_MX: BigDecimal, K_MAWF_DMDRDP_MX_B_REKU: BigDecimal, K_MAWF_F_DMDP_LDM: BigDecimal, K_MAWF_MDRMINHYS_DP_AUS: BigDecimal, K_MAWF_MDRMINHYS_DP_EIN: BigDecimal, K_MAWF_NODP_DKG: Array[String], S_MAWF_ActDashRekuDiff: String, S_MAWF_CP259987_Dash: String, S_MAWF_CP268856_Deakt: String, S_USE_ZKA_KOR_DSH_ENABLE_MAWF: String, Dmdp_mawf_vb: OutMeasurement, Md_rad_dash: OutMeasurement, St_MAWF_01: OutMeasurement, St_MAWF_01_B_nodash_mdrist: OutMeasurement, St_MAWF_01_B_nolsd_mdrist: OutMeasurement, St_MAWF_01_B_reset_dash_mawf: OutMeasurement, St_MAWF_01_B_reset_lsd_mawf: OutMeasurement, St_MAWF_01_B_set_dash_mawf: OutMeasurement, St_MAWF_01_B_set_lsd_mawf: OutMeasurement, St_dp_ber: OutMeasurement, St_mdar1: OutMeasurement, St_mdar1_B_dash: OutMeasurement, St_mdar1_B_kf_at: OutMeasurement): Unit = {
 ???
}

def P_MAWF_DASH_10ms(a2lBin: A2LBinAdapter): Unit = {

val CW_CONF_DASH: BigDecimal = a2lBin.readCharacteristicWithCast("CW_CONF_DASH")
val CW_STMDINFO_DASH: BigDecimal = a2lBin.readCharacteristicWithCast("CW_STMDINFO_DASH")
val CW_S_SPORT: BigDecimal = a2lBin.readCharacteristicWithCast("CW_S_SPORT")
val KF_MAWF_DMDP1: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_DMDP1")
val KF_MAWF_DMDP1A: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_DMDP1A")
val KF_MAWF_DMDP2: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_DMDP2")
val KF_MAWF_DMDP2A: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_DMDP2A")
val KF_MAWF_DMDP3: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_DMDP3")
val KF_MAWF_DMDP3A: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_DMDP3A")
val KF_MAWF_DMDPOFF: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_DMDPOFF")
val KF_MAWF_DMDPON: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_DMDPON")
val KF_MAWF_FDMDP3_MN: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_FDMDP3_MN")
val KF_MAWF_F_SPORT_2_DASH3: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_F_SPORT_2_DASH3")
val KF_MAWF_MDR_DP_O: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_MDR_DP_O")
val KF_MAWF_MDR_DP_OA: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_MDR_DP_OA")
val KF_MAWF_MDR_DP_U: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_MDR_DP_U")
val KF_MAWF_MDR_DP_UA: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_MDR_DP_UA")
val KF_MAWF_ZKDP3: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_ZKDP3")
val KF_MAWF_ZKDP3A: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_ZKDP3A")
val KL_MAWF_FDMDP1_BREMS: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MAWF_FDMDP1_BREMS")
val KL_MAWF_FDMDP2_BREMS: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MAWF_FDMDP2_BREMS")
val KL_MAWF_FDMDP3_BREMS: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MAWF_FDMDP3_BREMS")
val KL_MAWF_FDMDPA1: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MAWF_FDMDPA1")
val KL_MAWF_FDMDPA2: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MAWF_FDMDPA2")
val KL_MAWF_FDMDPA3: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MAWF_FDMDPA3")
val KL_MAWF_F_SPORT_DASH1: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MAWF_F_SPORT_DASH1")
val KL_MAWF_F_SPORT_DASH2: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MAWF_F_SPORT_DASH2")
val KL_MAWF_F_SPORT_DASH3: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MAWF_F_SPORT_DASH3")
val K_MAWF_AQMAX_SPS: BigDecimal = a2lBin.readCharacteristicWithCast("K_MAWF_AQMAX_SPS")
val K_MAWF_DMDRDP_MX: BigDecimal = a2lBin.readCharacteristicWithCast("K_MAWF_DMDRDP_MX")
val K_MAWF_DMDRDP_MX_B_REKU: BigDecimal = a2lBin.readCharacteristicWithCast("K_MAWF_DMDRDP_MX_B_REKU")
val K_MAWF_F_DMDP_LDM: BigDecimal = a2lBin.readCharacteristicWithCast("K_MAWF_F_DMDP_LDM")
val K_MAWF_MDRMINHYS_DP_AUS: BigDecimal = a2lBin.readCharacteristicWithCast("K_MAWF_MDRMINHYS_DP_AUS")
val K_MAWF_MDRMINHYS_DP_EIN: BigDecimal = a2lBin.readCharacteristicWithCast("K_MAWF_MDRMINHYS_DP_EIN")
val K_MAWF_NODP_DKG: Array[String] = a2lBin.readCharacteristicWithCast("K_MAWF_NODP_DKG")
val S_MAWF_ActDashRekuDiff: String = a2lBin.readCharacteristicWithCast("S_MAWF_ActDashRekuDiff")
val S_MAWF_CP259987_Dash: String = a2lBin.readCharacteristicWithCast("S_MAWF_CP259987_Dash")
val S_MAWF_CP268856_Deakt: String = a2lBin.readCharacteristicWithCast("S_MAWF_CP268856_Deakt")
val S_USE_ZKA_KOR_DSH_ENABLE_MAWF: String = a2lBin.readCharacteristicWithCast("S_USE_ZKA_KOR_DSH_ENABLE_MAWF")
val A_quer_abs: InMeasurement = a2lBin.measurement("A_quer_abs")
val BMWbdy_b_CluOp10_bo: InMeasurement = a2lBin.measurement("BMWbdy_b_CluOp10_bo")
val BMWmsa_stb_MsaDrr_ub: InMeasurement = a2lBin.measurement("BMWmsa_stb_MsaDrr_ub")
val BMWmsa_stb_MsaDrr_ub_B_fahrbereit: InMeasurement = a2lBin.measurement("BMWmsa_stb_MsaDrr_ub.B_fahrbereit")
val BMWmsa_stb_MsaDrr_ub_B_fahrbereit_pwf: InMeasurement = a2lBin.measurement("BMWmsa_stb_MsaDrr_ub.B_fahrbereit_pwf")
val BMWtqe_st_tq_LimSrc_ul: InMeasurement = a2lBin.measurement("BMWtqe_st_tq_LimSrc_ul")
val BMWtqw_tqw_AxcSumMaxDyn_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcSumMaxDyn_sw")
val BMWtqw_tqw_AxcSumMinDyn_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcSumMinDyn_sw")
val BMWtqw_tqw_AxcSumMinStat_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcSumMinStat_sw")
val B_sport_in_2: InMeasurement = a2lBin.measurement("B_sport_in_2")
val Gangi: InMeasurement = a2lBin.measurement("Gangi")
val I_ges_vh: InMeasurement = a2lBin.measurement("I_ges_vh")
val Md_rad_brems_antr_soll: InMeasurement = a2lBin.measurement("Md_rad_brems_antr_soll")
val Md_rad_fzdyn_int: InMeasurement = a2lBin.measurement("Md_rad_fzdyn_int")
val Md_rad_ini_dp: InMeasurement = a2lBin.measurement("Md_rad_ini_dp")
val Md_rad_lsd_dash: InMeasurement = a2lBin.measurement("Md_rad_lsd_dash")
val Md_rad_max_zka: InMeasurement = a2lBin.measurement("Md_rad_max_zka")
val Md_rad_min_zka: InMeasurement = a2lBin.measurement("Md_rad_min_zka")
val Md_rad_pedal_ap: InMeasurement = a2lBin.measurement("Md_rad_pedal_ap")
val Md_rad_schlepp_soll: InMeasurement = a2lBin.measurement("Md_rad_schlepp_soll")
val Md_rad_wunsch_begr: InMeasurement = a2lBin.measurement("Md_rad_wunsch_begr")
val Md_rad_wunsch_fas: InMeasurement = a2lBin.measurement("Md_rad_wunsch_fas")
val Md_rad_wunsch_grad: InMeasurement = a2lBin.measurement("Md_rad_wunsch_grad")
val Md_rad_wunsch_vb: InMeasurement = a2lBin.measurement("Md_rad_wunsch_vb")
val St_fw: InMeasurement = a2lBin.measurement("St_fw")
val St_fw_B_hschalt_komb: InMeasurement = a2lBin.measurement("St_fw.B_hschalt_komb")
val St_fw_B_ldm_akt04: InMeasurement = a2lBin.measurement("St_fw.B_ldm_akt04")
val St_fw_B_ldm_off: InMeasurement = a2lBin.measurement("St_fw.B_ldm_off")
val St_fw_B_ldm_offhs: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offhs")
val St_fw_B_ldm_offreg: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offreg")
val St_fw_B_ldm_offrs: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offrs")
val St_fw_B_rschalt_komb: InMeasurement = a2lBin.measurement("St_fw.B_rschalt_komb")
val St_getrdaten: InMeasurement = a2lBin.measurement("St_getrdaten")
val St_getrdaten_B_gangwechsel_gs: InMeasurement = a2lBin.measurement("St_getrdaten.B_gangwechsel_gs")
val St_kupp_dkg_plaus: InMeasurement = a2lBin.measurement("St_kupp_dkg_plaus")
val St_mdar0: InMeasurement = a2lBin.measurement("St_mdar0")
val St_mdar0_B_edp: InMeasurement = a2lBin.measurement("St_mdar0.B_edp")
val St_mdar0_B_elsd: InMeasurement = a2lBin.measurement("St_mdar0.B_elsd")
val St_mdar0_B_lsd: InMeasurement = a2lBin.measurement("St_mdar0.B_lsd")
val St_mdfw: InMeasurement = a2lBin.measurement("St_mdfw")
val St_mdfw_B_sld_akt: InMeasurement = a2lBin.measurement("St_mdfw.B_sld_akt")
val St_mdipmfw: InMeasurement = a2lBin.measurement("St_mdipmfw")
val St_mdipmfw_B_bst: InMeasurement = a2lBin.measurement("St_mdipmfw.B_bst")
val St_mdipmfw_B_emf_aktiv: InMeasurement = a2lBin.measurement("St_mdipmfw.B_emf_aktiv")
val St_mdipmfw_B_reku: InMeasurement = a2lBin.measurement("St_mdipmfw.B_reku")
val St_mdipmfw_B_schlepp: InMeasurement = a2lBin.measurement("St_mdipmfw.B_schlepp")
val St_mdldynf: InMeasurement = a2lBin.measurement("St_mdldynf")
val St_mdldynf_B_sport_in: InMeasurement = a2lBin.measurement("St_mdldynf.B_sport_in")
val V_fzg_fahrtricht: InMeasurement = a2lBin.measurement("V_fzg_fahrtricht")
val Var_at: InMeasurement = a2lBin.measurement("Var_at")
val Var_dkg: InMeasurement = a2lBin.measurement("Var_dkg")
val Zka_fak: InMeasurement = a2lBin.measurement("Zka_fak")
val Dmdp_mawf_vb: OutMeasurement = a2lBin.measurement("Dmdp_mawf_vb")
val Md_rad_dash: OutMeasurement = a2lBin.measurement("Md_rad_dash")
val St_MAWF_01: OutMeasurement = a2lBin.measurement("St_MAWF_01")
val St_MAWF_01_B_nodash_mdrist: OutMeasurement = a2lBin.measurement("St_MAWF_01.B_nodash_mdrist")
val St_MAWF_01_B_nolsd_mdrist: OutMeasurement = a2lBin.measurement("St_MAWF_01.B_nolsd_mdrist")
val St_MAWF_01_B_reset_dash_mawf: OutMeasurement = a2lBin.measurement("St_MAWF_01.B_reset_dash_mawf")
val St_MAWF_01_B_reset_lsd_mawf: OutMeasurement = a2lBin.measurement("St_MAWF_01.B_reset_lsd_mawf")
val St_MAWF_01_B_set_dash_mawf: OutMeasurement = a2lBin.measurement("St_MAWF_01.B_set_dash_mawf")
val St_MAWF_01_B_set_lsd_mawf: OutMeasurement = a2lBin.measurement("St_MAWF_01.B_set_lsd_mawf")
val St_dp_ber: OutMeasurement = a2lBin.measurement("St_dp_ber")
val St_mdar1: OutMeasurement = a2lBin.measurement("St_mdar1")
val St_mdar1_B_dash: OutMeasurement = a2lBin.measurement("St_mdar1.B_dash")
val St_mdar1_B_kf_at: OutMeasurement = a2lBin.measurement("St_mdar1.B_kf_at")

  P_MAWF_DASH_10ms(A_quer_abs, BMWbdy_b_CluOp10_bo, BMWmsa_stb_MsaDrr_ub, BMWmsa_stb_MsaDrr_ub_B_fahrbereit, BMWmsa_stb_MsaDrr_ub_B_fahrbereit_pwf, BMWtqe_st_tq_LimSrc_ul, BMWtqw_tqw_AxcSumMaxDyn_sw, BMWtqw_tqw_AxcSumMinDyn_sw, BMWtqw_tqw_AxcSumMinStat_sw, B_sport_in_2, Gangi, I_ges_vh, Md_rad_brems_antr_soll, Md_rad_fzdyn_int, Md_rad_ini_dp, Md_rad_lsd_dash, Md_rad_max_zka, Md_rad_min_zka, Md_rad_pedal_ap, Md_rad_schlepp_soll, Md_rad_wunsch_begr, Md_rad_wunsch_fas, Md_rad_wunsch_grad, Md_rad_wunsch_vb, St_fw, St_fw_B_hschalt_komb, St_fw_B_ldm_akt04, St_fw_B_ldm_off, St_fw_B_ldm_offhs, St_fw_B_ldm_offreg, St_fw_B_ldm_offrs, St_fw_B_rschalt_komb, St_getrdaten, St_getrdaten_B_gangwechsel_gs, St_kupp_dkg_plaus, St_mdar0, St_mdar0_B_edp, St_mdar0_B_elsd, St_mdar0_B_lsd, St_mdfw, St_mdfw_B_sld_akt, St_mdipmfw, St_mdipmfw_B_bst, St_mdipmfw_B_emf_aktiv, St_mdipmfw_B_reku, St_mdipmfw_B_schlepp, St_mdldynf, St_mdldynf_B_sport_in, V_fzg_fahrtricht, Var_at, Var_dkg, Zka_fak, CW_CONF_DASH, CW_STMDINFO_DASH, CW_S_SPORT, KF_MAWF_DMDP1, KF_MAWF_DMDP1A, KF_MAWF_DMDP2, KF_MAWF_DMDP2A, KF_MAWF_DMDP3, KF_MAWF_DMDP3A, KF_MAWF_DMDPOFF, KF_MAWF_DMDPON, KF_MAWF_FDMDP3_MN, KF_MAWF_F_SPORT_2_DASH3, KF_MAWF_MDR_DP_O, KF_MAWF_MDR_DP_OA, KF_MAWF_MDR_DP_U, KF_MAWF_MDR_DP_UA, KF_MAWF_ZKDP3, KF_MAWF_ZKDP3A, KL_MAWF_FDMDP1_BREMS, KL_MAWF_FDMDP2_BREMS, KL_MAWF_FDMDP3_BREMS, KL_MAWF_FDMDPA1, KL_MAWF_FDMDPA2, KL_MAWF_FDMDPA3, KL_MAWF_F_SPORT_DASH1, KL_MAWF_F_SPORT_DASH2, KL_MAWF_F_SPORT_DASH3, K_MAWF_AQMAX_SPS, K_MAWF_DMDRDP_MX, K_MAWF_DMDRDP_MX_B_REKU, K_MAWF_F_DMDP_LDM, K_MAWF_MDRMINHYS_DP_AUS, K_MAWF_MDRMINHYS_DP_EIN, K_MAWF_NODP_DKG, S_MAWF_ActDashRekuDiff, S_MAWF_CP259987_Dash, S_MAWF_CP268856_Deakt, S_USE_ZKA_KOR_DSH_ENABLE_MAWF, Dmdp_mawf_vb, Md_rad_dash, St_MAWF_01, St_MAWF_01_B_nodash_mdrist, St_MAWF_01_B_nolsd_mdrist, St_MAWF_01_B_reset_dash_mawf, St_MAWF_01_B_reset_lsd_mawf, St_MAWF_01_B_set_dash_mawf, St_MAWF_01_B_set_lsd_mawf, St_dp_ber, St_mdar1, St_mdar1_B_dash, St_mdar1_B_kf_at)
}


def P_MAWF_IN_10ms(A_quer_plaus: InMeasurement, BMWtqc_Rat_GbxWhlFrntAxl: InMeasurement, BMWtqc_Rat_GbxWhlReAxl: InMeasurement, BMWtqw_fac_StatLossHa_ub: InMeasurement, BMWtqw_tqw_AxcSumMaxDyn_sw: InMeasurement, BMWtqw_tqw_AxcSumMaxStat_sw: InMeasurement, BMWtqw_tqw_AxcSumMinDyn_sw: InMeasurement, BMWtqw_tqw_AxcSumMinStat_sw: InMeasurement, BMWtqw_tqw_StatLossHa_sw: InMeasurement, Md_rad_fzdyn_int: InMeasurement, Md_rad_wunsch_fas: InMeasurement, Mdk_w_f: InMeasurement, St_mdar1: InMeasurement, St_mdar1_B_dash: InMeasurement, St_mdar1_B_kf_at: InMeasurement, Status_iae_plaus: InMeasurement, Status_usecase_antr: InMeasurement, Var_at: InMeasurement, Var_dkg: InMeasurement, KF_I_GES_VH_AT: MapType[BigDecimal, BigDecimal, BigDecimal], KF_I_GES_VH_HS: MapType[BigDecimal, BigDecimal, BigDecimal], KL_MAWF_B_SPORT: CurveType[BigDecimal, BigDecimal], KL_MAWF_B_SPORT_2: CurveType[BigDecimal, BigDecimal], KL_MAWF_KFATHS_EIN: CurveType[BigDecimal, BigDecimal], S_VM_HA: String, A_quer_abs: OutMeasurement, B_sport_in_2: OutMeasurement, I_ges_vh: OutMeasurement, Md_rad_fzdyn_getr: OutMeasurement, Md_rad_ini_dp: OutMeasurement, Md_rad_ini_lsd: OutMeasurement, Md_rad_wunsch_begr: OutMeasurement, St_mdldynf: OutMeasurement, St_mdldynf_B_sport_in: OutMeasurement): Unit = {
 ???
}

def P_MAWF_IN_10ms(a2lBin: A2LBinAdapter): Unit = {

val KF_I_GES_VH_AT: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_I_GES_VH_AT")
val KF_I_GES_VH_HS: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_I_GES_VH_HS")
val KL_MAWF_B_SPORT: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MAWF_B_SPORT")
val KL_MAWF_B_SPORT_2: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MAWF_B_SPORT_2")
val KL_MAWF_KFATHS_EIN: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MAWF_KFATHS_EIN")
val S_VM_HA: String = a2lBin.readCharacteristicWithCast("S_VM_HA")
val A_quer_plaus: InMeasurement = a2lBin.measurement("A_quer_plaus")
val BMWtqc_Rat_GbxWhlFrntAxl: InMeasurement = a2lBin.measurement("BMWtqc_Rat_GbxWhlFrntAxl")
val BMWtqc_Rat_GbxWhlReAxl: InMeasurement = a2lBin.measurement("BMWtqc_Rat_GbxWhlReAxl")
val BMWtqw_fac_StatLossHa_ub: InMeasurement = a2lBin.measurement("BMWtqw_fac_StatLossHa_ub")
val BMWtqw_tqw_AxcSumMaxDyn_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcSumMaxDyn_sw")
val BMWtqw_tqw_AxcSumMaxStat_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcSumMaxStat_sw")
val BMWtqw_tqw_AxcSumMinDyn_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcSumMinDyn_sw")
val BMWtqw_tqw_AxcSumMinStat_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcSumMinStat_sw")
val BMWtqw_tqw_StatLossHa_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_StatLossHa_sw")
val Md_rad_fzdyn_int: InMeasurement = a2lBin.measurement("Md_rad_fzdyn_int")
val Md_rad_wunsch_fas: InMeasurement = a2lBin.measurement("Md_rad_wunsch_fas")
val Mdk_w_f: InMeasurement = a2lBin.measurement("Mdk_w_f")
val St_mdar1: InMeasurement = a2lBin.measurement("St_mdar1")
val St_mdar1_B_dash: InMeasurement = a2lBin.measurement("St_mdar1.B_dash")
val St_mdar1_B_kf_at: InMeasurement = a2lBin.measurement("St_mdar1.B_kf_at")
val Status_iae_plaus: InMeasurement = a2lBin.measurement("Status_iae_plaus")
val Status_usecase_antr: InMeasurement = a2lBin.measurement("Status_usecase_antr")
val Var_at: InMeasurement = a2lBin.measurement("Var_at")
val Var_dkg: InMeasurement = a2lBin.measurement("Var_dkg")
val A_quer_abs: OutMeasurement = a2lBin.measurement("A_quer_abs")
val B_sport_in_2: OutMeasurement = a2lBin.measurement("B_sport_in_2")
val I_ges_vh: OutMeasurement = a2lBin.measurement("I_ges_vh")
val Md_rad_fzdyn_getr: OutMeasurement = a2lBin.measurement("Md_rad_fzdyn_getr")
val Md_rad_ini_dp: OutMeasurement = a2lBin.measurement("Md_rad_ini_dp")
val Md_rad_ini_lsd: OutMeasurement = a2lBin.measurement("Md_rad_ini_lsd")
val Md_rad_wunsch_begr: OutMeasurement = a2lBin.measurement("Md_rad_wunsch_begr")
val St_mdldynf: OutMeasurement = a2lBin.measurement("St_mdldynf")
val St_mdldynf_B_sport_in: OutMeasurement = a2lBin.measurement("St_mdldynf.B_sport_in")

  P_MAWF_IN_10ms(A_quer_plaus, BMWtqc_Rat_GbxWhlFrntAxl, BMWtqc_Rat_GbxWhlReAxl, BMWtqw_fac_StatLossHa_ub, BMWtqw_tqw_AxcSumMaxDyn_sw, BMWtqw_tqw_AxcSumMaxStat_sw, BMWtqw_tqw_AxcSumMinDyn_sw, BMWtqw_tqw_AxcSumMinStat_sw, BMWtqw_tqw_StatLossHa_sw, Md_rad_fzdyn_int, Md_rad_wunsch_fas, Mdk_w_f, St_mdar1, St_mdar1_B_dash, St_mdar1_B_kf_at, Status_iae_plaus, Status_usecase_antr, Var_at, Var_dkg, KF_I_GES_VH_AT, KF_I_GES_VH_HS, KL_MAWF_B_SPORT, KL_MAWF_B_SPORT_2, KL_MAWF_KFATHS_EIN, S_VM_HA, A_quer_abs, B_sport_in_2, I_ges_vh, Md_rad_fzdyn_getr, Md_rad_ini_dp, Md_rad_ini_lsd, Md_rad_wunsch_begr, St_mdldynf, St_mdldynf_B_sport_in)
}


def P_MAWF_LSD_10ms(BMWbdy_b_CluOp10_bo: InMeasurement, BMWmsa_stb_MsaDrr_ub: InMeasurement, BMWmsa_stb_MsaDrr_ub_B_fahrbereit: InMeasurement, BMWmsa_stb_MsaDrr_ub_B_fahrbereit_pwf: InMeasurement, BMWosc_v_VehMax_uw: InMeasurement, BMWtqe_st_tq_LimSrc_ul: InMeasurement, BMWtqw_tqw_AxcSumMaxStat_sw: InMeasurement, BMWtqw_tqw_AxcSumMinStat_sw: InMeasurement, B_sport_in_2: InMeasurement, Fahrstufe_antrieb: InMeasurement, Gangi: InMeasurement, I_ges_vh: InMeasurement, Md_rad_fzdyn_int: InMeasurement, Md_rad_ini_lsd: InMeasurement, Md_rad_lsd_dash: InMeasurement, Md_rad_max_zka: InMeasurement, Md_rad_min_zka: InMeasurement, Md_rad_schlepp_soll: InMeasurement, Md_rad_wunsch_begr: InMeasurement, Md_rad_wunsch_fas: InMeasurement, Md_rad_wunsch_vb: InMeasurement, Neig_l_plaus: InMeasurement, St_MAWF_01: InMeasurement, St_MAWF_01_B_nodash_mdrist: InMeasurement, St_MAWF_01_B_nolsd_mdrist: InMeasurement, St_MAWF_01_B_reset_dash_mawf: InMeasurement, St_MAWF_01_B_reset_lsd_mawf: InMeasurement, St_MAWF_01_B_set_dash_mawf: InMeasurement, St_MAWF_01_B_set_lsd_mawf: InMeasurement, St_fw: InMeasurement, St_fw_B_hschalt_komb: InMeasurement, St_fw_B_ldm_akt04: InMeasurement, St_fw_B_ldm_off: InMeasurement, St_fw_B_ldm_offhs: InMeasurement, St_fw_B_ldm_offreg: InMeasurement, St_fw_B_ldm_offrs: InMeasurement, St_fw_B_rschalt_komb: InMeasurement, St_getrdaten: InMeasurement, St_getrdaten_B_gangwechsel_gs: InMeasurement, St_kupp_dkg_plaus: InMeasurement, St_mdar1: InMeasurement, St_mdar1_B_dash: InMeasurement, St_mdar1_B_kf_at: InMeasurement, St_mdfw: InMeasurement, St_mdfw_B_sld_akt: InMeasurement, St_mdldynf: InMeasurement, St_mdldynf_B_sport_in: InMeasurement, V_fzg_fahrtricht: InMeasurement, Var_at: InMeasurement, Var_dkg: InMeasurement, Zka_fak: InMeasurement, CW_CONF_LSD: BigDecimal, CW_STMDINFO_LSD: BigDecimal, KF_F_MD_WUNSCH_LSD3: MapType[BigDecimal, BigDecimal, BigDecimal], KF_F_MD_WUNSCH_NEIG_LSD3: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_DMLS1UD: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_DMLS1UDA: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_DMLS2UD: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_DMLS2UDA: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_DMLS3UD: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_DMLS3UDA: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_DMLSDOFF: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_DMLSDON: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_F_SPORT_2_LS3: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_MDR_LS_O: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_MDR_LS_OA: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_MDR_LS_U: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_MDR_LS_UA: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_ZKLS3UD: MapType[BigDecimal, BigDecimal, BigDecimal], KF_MAWF_ZKLS3UDA: MapType[BigDecimal, BigDecimal, BigDecimal], KL_F_MD_WUNSCH_LSD1: CurveType[BigDecimal, BigDecimal], KL_F_MD_WUNSCH_LSD2: CurveType[BigDecimal, BigDecimal], KL_MAWF_F_SPORT_LS1: CurveType[BigDecimal, BigDecimal], KL_MAWF_F_SPORT_LS2: CurveType[BigDecimal, BigDecimal], KL_MAWF_F_SPORT_LS3: CurveType[BigDecimal, BigDecimal], K_MAWF_DELTA_V_MAX: BigDecimal, K_MAWF_DMDRLSD_MN: BigDecimal, K_MAWF_F_DMLSD_LDM: BigDecimal, K_MAWF_MDRMINHYS_LSD_AUS: BigDecimal, K_MAWF_MDRMINHYS_LSD_EIN: BigDecimal, K_MAWF_VERZ_MDR_LSD: BigDecimal, K_MD_RAD_SCHLEPP_DELTA_MAWF: BigDecimal, S_MAWF_ActLsdRekuDiff: String, S_MAWF_CP268856_Schub3: String, S_MAWF_CP270566_LSD: String, S_MAWF_LSDRESETDEAKT_DISABLE: String, S_USE_FZDYN4LSD_MAWF: String, S_USE_ZKA_KOR_LSD_ENABLE_MAWF: String, CW_S_SPORT: BigDecimal, S_MAWF_CP268856_Deakt: String, Dmls_mawf_vb: OutMeasurement, Md_rad_lsd: OutMeasurement, St_mdar0: OutMeasurement, St_mdar0_B_edp: OutMeasurement, St_mdar0_B_elsd: OutMeasurement, St_mdar0_B_lsd: OutMeasurement): Unit = {
 ???
}

def P_MAWF_LSD_10ms(a2lBin: A2LBinAdapter): Unit = {

val CW_CONF_LSD: BigDecimal = a2lBin.readCharacteristicWithCast("CW_CONF_LSD")
val CW_STMDINFO_LSD: BigDecimal = a2lBin.readCharacteristicWithCast("CW_STMDINFO_LSD")
val KF_F_MD_WUNSCH_LSD3: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_F_MD_WUNSCH_LSD3")
val KF_F_MD_WUNSCH_NEIG_LSD3: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_F_MD_WUNSCH_NEIG_LSD3")
val KF_MAWF_DMLS1UD: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_DMLS1UD")
val KF_MAWF_DMLS1UDA: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_DMLS1UDA")
val KF_MAWF_DMLS2UD: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_DMLS2UD")
val KF_MAWF_DMLS2UDA: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_DMLS2UDA")
val KF_MAWF_DMLS3UD: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_DMLS3UD")
val KF_MAWF_DMLS3UDA: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_DMLS3UDA")
val KF_MAWF_DMLSDOFF: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_DMLSDOFF")
val KF_MAWF_DMLSDON: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_DMLSDON")
val KF_MAWF_F_SPORT_2_LS3: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_F_SPORT_2_LS3")
val KF_MAWF_MDR_LS_O: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_MDR_LS_O")
val KF_MAWF_MDR_LS_OA: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_MDR_LS_OA")
val KF_MAWF_MDR_LS_U: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_MDR_LS_U")
val KF_MAWF_MDR_LS_UA: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_MDR_LS_UA")
val KF_MAWF_ZKLS3UD: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_ZKLS3UD")
val KF_MAWF_ZKLS3UDA: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_MAWF_ZKLS3UDA")
val KL_F_MD_WUNSCH_LSD1: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_F_MD_WUNSCH_LSD1")
val KL_F_MD_WUNSCH_LSD2: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_F_MD_WUNSCH_LSD2")
val KL_MAWF_F_SPORT_LS1: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MAWF_F_SPORT_LS1")
val KL_MAWF_F_SPORT_LS2: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MAWF_F_SPORT_LS2")
val KL_MAWF_F_SPORT_LS3: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MAWF_F_SPORT_LS3")
val K_MAWF_DELTA_V_MAX: BigDecimal = a2lBin.readCharacteristicWithCast("K_MAWF_DELTA_V_MAX")
val K_MAWF_DMDRLSD_MN: BigDecimal = a2lBin.readCharacteristicWithCast("K_MAWF_DMDRLSD_MN")
val K_MAWF_F_DMLSD_LDM: BigDecimal = a2lBin.readCharacteristicWithCast("K_MAWF_F_DMLSD_LDM")
val K_MAWF_MDRMINHYS_LSD_AUS: BigDecimal = a2lBin.readCharacteristicWithCast("K_MAWF_MDRMINHYS_LSD_AUS")
val K_MAWF_MDRMINHYS_LSD_EIN: BigDecimal = a2lBin.readCharacteristicWithCast("K_MAWF_MDRMINHYS_LSD_EIN")
val K_MAWF_VERZ_MDR_LSD: BigDecimal = a2lBin.readCharacteristicWithCast("K_MAWF_VERZ_MDR_LSD")
val K_MD_RAD_SCHLEPP_DELTA_MAWF: BigDecimal = a2lBin.readCharacteristicWithCast("K_MD_RAD_SCHLEPP_DELTA_MAWF")
val S_MAWF_ActLsdRekuDiff: String = a2lBin.readCharacteristicWithCast("S_MAWF_ActLsdRekuDiff")
val S_MAWF_CP268856_Schub3: String = a2lBin.readCharacteristicWithCast("S_MAWF_CP268856_Schub3")
val S_MAWF_CP270566_LSD: String = a2lBin.readCharacteristicWithCast("S_MAWF_CP270566_LSD")
val S_MAWF_LSDRESETDEAKT_DISABLE: String = a2lBin.readCharacteristicWithCast("S_MAWF_LSDRESETDEAKT_DISABLE")
val S_USE_FZDYN4LSD_MAWF: String = a2lBin.readCharacteristicWithCast("S_USE_FZDYN4LSD_MAWF")
val S_USE_ZKA_KOR_LSD_ENABLE_MAWF: String = a2lBin.readCharacteristicWithCast("S_USE_ZKA_KOR_LSD_ENABLE_MAWF")
val CW_S_SPORT: BigDecimal = a2lBin.readCharacteristicWithCast("CW_S_SPORT")
val S_MAWF_CP268856_Deakt: String = a2lBin.readCharacteristicWithCast("S_MAWF_CP268856_Deakt")
val BMWbdy_b_CluOp10_bo: InMeasurement = a2lBin.measurement("BMWbdy_b_CluOp10_bo")
val BMWmsa_stb_MsaDrr_ub: InMeasurement = a2lBin.measurement("BMWmsa_stb_MsaDrr_ub")
val BMWmsa_stb_MsaDrr_ub_B_fahrbereit: InMeasurement = a2lBin.measurement("BMWmsa_stb_MsaDrr_ub.B_fahrbereit")
val BMWmsa_stb_MsaDrr_ub_B_fahrbereit_pwf: InMeasurement = a2lBin.measurement("BMWmsa_stb_MsaDrr_ub.B_fahrbereit_pwf")
val BMWosc_v_VehMax_uw: InMeasurement = a2lBin.measurement("BMWosc_v_VehMax_uw")
val BMWtqe_st_tq_LimSrc_ul: InMeasurement = a2lBin.measurement("BMWtqe_st_tq_LimSrc_ul")
val BMWtqw_tqw_AxcSumMaxStat_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcSumMaxStat_sw")
val BMWtqw_tqw_AxcSumMinStat_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcSumMinStat_sw")
val B_sport_in_2: InMeasurement = a2lBin.measurement("B_sport_in_2")
val Fahrstufe_antrieb: InMeasurement = a2lBin.measurement("Fahrstufe_antrieb")
val Gangi: InMeasurement = a2lBin.measurement("Gangi")
val I_ges_vh: InMeasurement = a2lBin.measurement("I_ges_vh")
val Md_rad_fzdyn_int: InMeasurement = a2lBin.measurement("Md_rad_fzdyn_int")
val Md_rad_ini_lsd: InMeasurement = a2lBin.measurement("Md_rad_ini_lsd")
val Md_rad_lsd_dash: InMeasurement = a2lBin.measurement("Md_rad_lsd_dash")
val Md_rad_max_zka: InMeasurement = a2lBin.measurement("Md_rad_max_zka")
val Md_rad_min_zka: InMeasurement = a2lBin.measurement("Md_rad_min_zka")
val Md_rad_schlepp_soll: InMeasurement = a2lBin.measurement("Md_rad_schlepp_soll")
val Md_rad_wunsch_begr: InMeasurement = a2lBin.measurement("Md_rad_wunsch_begr")
val Md_rad_wunsch_fas: InMeasurement = a2lBin.measurement("Md_rad_wunsch_fas")
val Md_rad_wunsch_vb: InMeasurement = a2lBin.measurement("Md_rad_wunsch_vb")
val Neig_l_plaus: InMeasurement = a2lBin.measurement("Neig_l_plaus")
val St_MAWF_01: InMeasurement = a2lBin.measurement("St_MAWF_01")
val St_MAWF_01_B_nodash_mdrist: InMeasurement = a2lBin.measurement("St_MAWF_01.B_nodash_mdrist")
val St_MAWF_01_B_nolsd_mdrist: InMeasurement = a2lBin.measurement("St_MAWF_01.B_nolsd_mdrist")
val St_MAWF_01_B_reset_dash_mawf: InMeasurement = a2lBin.measurement("St_MAWF_01.B_reset_dash_mawf")
val St_MAWF_01_B_reset_lsd_mawf: InMeasurement = a2lBin.measurement("St_MAWF_01.B_reset_lsd_mawf")
val St_MAWF_01_B_set_dash_mawf: InMeasurement = a2lBin.measurement("St_MAWF_01.B_set_dash_mawf")
val St_MAWF_01_B_set_lsd_mawf: InMeasurement = a2lBin.measurement("St_MAWF_01.B_set_lsd_mawf")
val St_fw: InMeasurement = a2lBin.measurement("St_fw")
val St_fw_B_hschalt_komb: InMeasurement = a2lBin.measurement("St_fw.B_hschalt_komb")
val St_fw_B_ldm_akt04: InMeasurement = a2lBin.measurement("St_fw.B_ldm_akt04")
val St_fw_B_ldm_off: InMeasurement = a2lBin.measurement("St_fw.B_ldm_off")
val St_fw_B_ldm_offhs: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offhs")
val St_fw_B_ldm_offreg: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offreg")
val St_fw_B_ldm_offrs: InMeasurement = a2lBin.measurement("St_fw.B_ldm_offrs")
val St_fw_B_rschalt_komb: InMeasurement = a2lBin.measurement("St_fw.B_rschalt_komb")
val St_getrdaten: InMeasurement = a2lBin.measurement("St_getrdaten")
val St_getrdaten_B_gangwechsel_gs: InMeasurement = a2lBin.measurement("St_getrdaten.B_gangwechsel_gs")
val St_kupp_dkg_plaus: InMeasurement = a2lBin.measurement("St_kupp_dkg_plaus")
val St_mdar1: InMeasurement = a2lBin.measurement("St_mdar1")
val St_mdar1_B_dash: InMeasurement = a2lBin.measurement("St_mdar1.B_dash")
val St_mdar1_B_kf_at: InMeasurement = a2lBin.measurement("St_mdar1.B_kf_at")
val St_mdfw: InMeasurement = a2lBin.measurement("St_mdfw")
val St_mdfw_B_sld_akt: InMeasurement = a2lBin.measurement("St_mdfw.B_sld_akt")
val St_mdldynf: InMeasurement = a2lBin.measurement("St_mdldynf")
val St_mdldynf_B_sport_in: InMeasurement = a2lBin.measurement("St_mdldynf.B_sport_in")
val V_fzg_fahrtricht: InMeasurement = a2lBin.measurement("V_fzg_fahrtricht")
val Var_at: InMeasurement = a2lBin.measurement("Var_at")
val Var_dkg: InMeasurement = a2lBin.measurement("Var_dkg")
val Zka_fak: InMeasurement = a2lBin.measurement("Zka_fak")
val Dmls_mawf_vb: OutMeasurement = a2lBin.measurement("Dmls_mawf_vb")
val Md_rad_lsd: OutMeasurement = a2lBin.measurement("Md_rad_lsd")
val St_mdar0: OutMeasurement = a2lBin.measurement("St_mdar0")
val St_mdar0_B_edp: OutMeasurement = a2lBin.measurement("St_mdar0.B_edp")
val St_mdar0_B_elsd: OutMeasurement = a2lBin.measurement("St_mdar0.B_elsd")
val St_mdar0_B_lsd: OutMeasurement = a2lBin.measurement("St_mdar0.B_lsd")

  P_MAWF_LSD_10ms(BMWbdy_b_CluOp10_bo, BMWmsa_stb_MsaDrr_ub, BMWmsa_stb_MsaDrr_ub_B_fahrbereit, BMWmsa_stb_MsaDrr_ub_B_fahrbereit_pwf, BMWosc_v_VehMax_uw, BMWtqe_st_tq_LimSrc_ul, BMWtqw_tqw_AxcSumMaxStat_sw, BMWtqw_tqw_AxcSumMinStat_sw, B_sport_in_2, Fahrstufe_antrieb, Gangi, I_ges_vh, Md_rad_fzdyn_int, Md_rad_ini_lsd, Md_rad_lsd_dash, Md_rad_max_zka, Md_rad_min_zka, Md_rad_schlepp_soll, Md_rad_wunsch_begr, Md_rad_wunsch_fas, Md_rad_wunsch_vb, Neig_l_plaus, St_MAWF_01, St_MAWF_01_B_nodash_mdrist, St_MAWF_01_B_nolsd_mdrist, St_MAWF_01_B_reset_dash_mawf, St_MAWF_01_B_reset_lsd_mawf, St_MAWF_01_B_set_dash_mawf, St_MAWF_01_B_set_lsd_mawf, St_fw, St_fw_B_hschalt_komb, St_fw_B_ldm_akt04, St_fw_B_ldm_off, St_fw_B_ldm_offhs, St_fw_B_ldm_offreg, St_fw_B_ldm_offrs, St_fw_B_rschalt_komb, St_getrdaten, St_getrdaten_B_gangwechsel_gs, St_kupp_dkg_plaus, St_mdar1, St_mdar1_B_dash, St_mdar1_B_kf_at, St_mdfw, St_mdfw_B_sld_akt, St_mdldynf, St_mdldynf_B_sport_in, V_fzg_fahrtricht, Var_at, Var_dkg, Zka_fak, CW_CONF_LSD, CW_STMDINFO_LSD, KF_F_MD_WUNSCH_LSD3, KF_F_MD_WUNSCH_NEIG_LSD3, KF_MAWF_DMLS1UD, KF_MAWF_DMLS1UDA, KF_MAWF_DMLS2UD, KF_MAWF_DMLS2UDA, KF_MAWF_DMLS3UD, KF_MAWF_DMLS3UDA, KF_MAWF_DMLSDOFF, KF_MAWF_DMLSDON, KF_MAWF_F_SPORT_2_LS3, KF_MAWF_MDR_LS_O, KF_MAWF_MDR_LS_OA, KF_MAWF_MDR_LS_U, KF_MAWF_MDR_LS_UA, KF_MAWF_ZKLS3UD, KF_MAWF_ZKLS3UDA, KL_F_MD_WUNSCH_LSD1, KL_F_MD_WUNSCH_LSD2, KL_MAWF_F_SPORT_LS1, KL_MAWF_F_SPORT_LS2, KL_MAWF_F_SPORT_LS3, K_MAWF_DELTA_V_MAX, K_MAWF_DMDRLSD_MN, K_MAWF_F_DMLSD_LDM, K_MAWF_MDRMINHYS_LSD_AUS, K_MAWF_MDRMINHYS_LSD_EIN, K_MAWF_VERZ_MDR_LSD, K_MD_RAD_SCHLEPP_DELTA_MAWF, S_MAWF_ActLsdRekuDiff, S_MAWF_CP268856_Schub3, S_MAWF_CP270566_LSD, S_MAWF_LSDRESETDEAKT_DISABLE, S_USE_FZDYN4LSD_MAWF, S_USE_ZKA_KOR_LSD_ENABLE_MAWF, CW_S_SPORT, S_MAWF_CP268856_Deakt, Dmls_mawf_vb, Md_rad_lsd, St_mdar0, St_mdar0_B_edp, St_mdar0_B_elsd, St_mdar0_B_lsd)
}


def P_MAWF_OUT_10ms(Dm_rad_figa: InMeasurement, Dmdp_mawf_vb: InMeasurement, Dmls_mawf_vb: InMeasurement, Md_rad_dash: InMeasurement, Md_rad_fzdyn: InMeasurement, Md_rad_lsd: InMeasurement, Md_rad_max_zka: InMeasurement, Md_rad_wunsch_vb: InMeasurement, St_mdar0: InMeasurement, St_mdar0_B_edp: InMeasurement, St_mdar0_B_elsd: InMeasurement, St_mdar0_B_lsd: InMeasurement, St_mdar1: InMeasurement, St_mdar1_B_dash: InMeasurement, St_mdar1_B_kf_at: InMeasurement, KL_MD_MIN_FIGA: CurveType[BigDecimal, BigDecimal], K_DM_RAD_FIGA_C: String, K_DM_RAD_FIGA_ERR: BigDecimal, K_DM_RAD_FIGA_HYSLSP: BigDecimal, K_DM_RAD_FIGA_HYSRSP: BigDecimal, K_DM_RAD_FIGA_STBY: BigDecimal, K_DM_RAD_FIGA_V: BigDecimal, K_MAWF_DMDRLSDFIGA_MN: BigDecimal, B_figa_ls_mawf: OutMeasurement, Dm_rad_extlim_max_mawf: OutMeasurement, Dmdp_mawf: OutMeasurement, Dmls_mawf: OutMeasurement, Md_rad_fzdyn_vb: OutMeasurement, Md_rad_lsd_dash: OutMeasurement): Unit = {
 ???
}

def P_MAWF_OUT_10ms(a2lBin: A2LBinAdapter): Unit = {

val KL_MD_MIN_FIGA: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MD_MIN_FIGA")
val K_DM_RAD_FIGA_C: String = a2lBin.readCharacteristicWithCast("K_DM_RAD_FIGA_C")
val K_DM_RAD_FIGA_ERR: BigDecimal = a2lBin.readCharacteristicWithCast("K_DM_RAD_FIGA_ERR")
val K_DM_RAD_FIGA_HYSLSP: BigDecimal = a2lBin.readCharacteristicWithCast("K_DM_RAD_FIGA_HYSLSP")
val K_DM_RAD_FIGA_HYSRSP: BigDecimal = a2lBin.readCharacteristicWithCast("K_DM_RAD_FIGA_HYSRSP")
val K_DM_RAD_FIGA_STBY: BigDecimal = a2lBin.readCharacteristicWithCast("K_DM_RAD_FIGA_STBY")
val K_DM_RAD_FIGA_V: BigDecimal = a2lBin.readCharacteristicWithCast("K_DM_RAD_FIGA_V")
val K_MAWF_DMDRLSDFIGA_MN: BigDecimal = a2lBin.readCharacteristicWithCast("K_MAWF_DMDRLSDFIGA_MN")
val Dm_rad_figa: InMeasurement = a2lBin.measurement("Dm_rad_figa")
val Dmdp_mawf_vb: InMeasurement = a2lBin.measurement("Dmdp_mawf_vb")
val Dmls_mawf_vb: InMeasurement = a2lBin.measurement("Dmls_mawf_vb")
val Md_rad_dash: InMeasurement = a2lBin.measurement("Md_rad_dash")
val Md_rad_fzdyn: InMeasurement = a2lBin.measurement("Md_rad_fzdyn")
val Md_rad_lsd: InMeasurement = a2lBin.measurement("Md_rad_lsd")
val Md_rad_max_zka: InMeasurement = a2lBin.measurement("Md_rad_max_zka")
val Md_rad_wunsch_vb: InMeasurement = a2lBin.measurement("Md_rad_wunsch_vb")
val St_mdar0: InMeasurement = a2lBin.measurement("St_mdar0")
val St_mdar0_B_edp: InMeasurement = a2lBin.measurement("St_mdar0.B_edp")
val St_mdar0_B_elsd: InMeasurement = a2lBin.measurement("St_mdar0.B_elsd")
val St_mdar0_B_lsd: InMeasurement = a2lBin.measurement("St_mdar0.B_lsd")
val St_mdar1: InMeasurement = a2lBin.measurement("St_mdar1")
val St_mdar1_B_dash: InMeasurement = a2lBin.measurement("St_mdar1.B_dash")
val St_mdar1_B_kf_at: InMeasurement = a2lBin.measurement("St_mdar1.B_kf_at")
val B_figa_ls_mawf: OutMeasurement = a2lBin.measurement("B_figa_ls_mawf")
val Dm_rad_extlim_max_mawf: OutMeasurement = a2lBin.measurement("Dm_rad_extlim_max_mawf")
val Dmdp_mawf: OutMeasurement = a2lBin.measurement("Dmdp_mawf")
val Dmls_mawf: OutMeasurement = a2lBin.measurement("Dmls_mawf")
val Md_rad_fzdyn_vb: OutMeasurement = a2lBin.measurement("Md_rad_fzdyn_vb")
val Md_rad_lsd_dash: OutMeasurement = a2lBin.measurement("Md_rad_lsd_dash")

  P_MAWF_OUT_10ms(Dm_rad_figa, Dmdp_mawf_vb, Dmls_mawf_vb, Md_rad_dash, Md_rad_fzdyn, Md_rad_lsd, Md_rad_max_zka, Md_rad_wunsch_vb, St_mdar0, St_mdar0_B_edp, St_mdar0_B_elsd, St_mdar0_B_lsd, St_mdar1, St_mdar1_B_dash, St_mdar1_B_kf_at, KL_MD_MIN_FIGA, K_DM_RAD_FIGA_C, K_DM_RAD_FIGA_ERR, K_DM_RAD_FIGA_HYSLSP, K_DM_RAD_FIGA_HYSRSP, K_DM_RAD_FIGA_STBY, K_DM_RAD_FIGA_V, K_MAWF_DMDRLSDFIGA_MN, B_figa_ls_mawf, Dm_rad_extlim_max_mawf, Dmdp_mawf, Dmls_mawf, Md_rad_fzdyn_vb, Md_rad_lsd_dash)
}


def P_MAW_10ms(A_quer_plaus: InMeasurement, Md_rad_dynverl: InMeasurement, Md_rad_ges_max_fd: InMeasurement, Md_rad_ges_min_fd: InMeasurement, Md_rad_max: InMeasurement, Md_rad_pedal: InMeasurement, Md_rad_soll_dcc: InMeasurement, Md_rad_soll_sld: InMeasurement, Pwg_ist: InMeasurement, Pwg_ist_mafw: InMeasurement, St_anman: InMeasurement, St_anman_B_antrieb: InMeasurement, St_anman_B_getreten: InMeasurement, St_anman_B_kein_gang: InMeasurement, St_anman_B_kupp_int: InMeasurement, St_antrieb_soll: InMeasurement, St_mdrdmk: InMeasurement, St_mdrdmk_B_fas_dcc: InMeasurement, St_mdrdmk_B_fas_sld: InMeasurement, Status_antrieb_ist: InMeasurement, W_radlenk: InMeasurement, KF_FAK_FGA_LIM_VM_START: MapType[BigDecimal, BigDecimal, BigDecimal], KL_MANU_MD_RAD_MIN_FD: CurveType[BigDecimal, BigDecimal], K_DELAY_MD_RAD_WUNSCH_GRAD: BigDecimal, K_DT_MAX_FGA_VMSTART: BigDecimal, K_F_FADE_FGA_INTERPRET_DN: BigDecimal, K_F_FADE_FGA_INTERPRET_UP: BigDecimal, K_LDM_PWGMAX: BigDecimal, K_MAW_PWG_SLD_AUSSTIEG: BigDecimal, K_MDIPM_HYS: BigDecimal, K_MD_RAD_FIGA_MIN_STBY: BigDecimal, K_MD_RAD_GES_FGA_MIN: BigDecimal, K_TD_MAW_PWG_SLD_AUSSTIEG: BigDecimal, MD_RAD_GES_MAX_FD_C: String, MD_RAD_GES_MAX_FD_V: BigDecimal, MD_RAD_GES_MIN_FD_C: String, MD_RAD_GES_MIN_FD_V: BigDecimal, S_MAW_SLD_AUSSTIEG_RAD_MAX: String, S_USE_AXHY_FGA_INTERPRET: String, BMWtqw_cw_CmpLossLimExt_C: BigDecimal, Md_rad_wunsch_fas: OutMeasurement, Md_rad_wunsch_grad: OutMeasurement, Md_rad_wunsch_vb: OutMeasurement, St_fw: OutMeasurement, St_fw_B_hschalt_komb: OutMeasurement, St_fw_B_ldm_akt04: OutMeasurement, St_fw_B_ldm_off: OutMeasurement, St_fw_B_ldm_offhs: OutMeasurement, St_fw_B_ldm_offreg: OutMeasurement, St_fw_B_ldm_offrs: OutMeasurement, St_fw_B_rschalt_komb: OutMeasurement, St_mdfw: OutMeasurement, St_mdfw_B_sld_akt: OutMeasurement, St_mdinfo_maw: OutMeasurement, St_mdinfo_maw_B_fd_max_akt: OutMeasurement, St_mdinfo_maw_B_fd_min_akt: OutMeasurement): Unit = {
 ???
}

def P_MAW_10ms(a2lBin: A2LBinAdapter): Unit = {

val KF_FAK_FGA_LIM_VM_START: MapType[BigDecimal, BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KF_FAK_FGA_LIM_VM_START")
val KL_MANU_MD_RAD_MIN_FD: CurveType[BigDecimal, BigDecimal] = a2lBin.readCharacteristicWithCast("KL_MANU_MD_RAD_MIN_FD")
val K_DELAY_MD_RAD_WUNSCH_GRAD: BigDecimal = a2lBin.readCharacteristicWithCast("K_DELAY_MD_RAD_WUNSCH_GRAD")
val K_DT_MAX_FGA_VMSTART: BigDecimal = a2lBin.readCharacteristicWithCast("K_DT_MAX_FGA_VMSTART")
val K_F_FADE_FGA_INTERPRET_DN: BigDecimal = a2lBin.readCharacteristicWithCast("K_F_FADE_FGA_INTERPRET_DN")
val K_F_FADE_FGA_INTERPRET_UP: BigDecimal = a2lBin.readCharacteristicWithCast("K_F_FADE_FGA_INTERPRET_UP")
val K_LDM_PWGMAX: BigDecimal = a2lBin.readCharacteristicWithCast("K_LDM_PWGMAX")
val K_MAW_PWG_SLD_AUSSTIEG: BigDecimal = a2lBin.readCharacteristicWithCast("K_MAW_PWG_SLD_AUSSTIEG")
val K_MDIPM_HYS: BigDecimal = a2lBin.readCharacteristicWithCast("K_MDIPM_HYS")
val K_MD_RAD_FIGA_MIN_STBY: BigDecimal = a2lBin.readCharacteristicWithCast("K_MD_RAD_FIGA_MIN_STBY")
val K_MD_RAD_GES_FGA_MIN: BigDecimal = a2lBin.readCharacteristicWithCast("K_MD_RAD_GES_FGA_MIN")
val K_TD_MAW_PWG_SLD_AUSSTIEG: BigDecimal = a2lBin.readCharacteristicWithCast("K_TD_MAW_PWG_SLD_AUSSTIEG")
val MD_RAD_GES_MAX_FD_C: String = a2lBin.readCharacteristicWithCast("MD_RAD_GES_MAX_FD_C")
val MD_RAD_GES_MAX_FD_V: BigDecimal = a2lBin.readCharacteristicWithCast("MD_RAD_GES_MAX_FD_V")
val MD_RAD_GES_MIN_FD_C: String = a2lBin.readCharacteristicWithCast("MD_RAD_GES_MIN_FD_C")
val MD_RAD_GES_MIN_FD_V: BigDecimal = a2lBin.readCharacteristicWithCast("MD_RAD_GES_MIN_FD_V")
val S_MAW_SLD_AUSSTIEG_RAD_MAX: String = a2lBin.readCharacteristicWithCast("S_MAW_SLD_AUSSTIEG_RAD_MAX")
val S_USE_AXHY_FGA_INTERPRET: String = a2lBin.readCharacteristicWithCast("S_USE_AXHY_FGA_INTERPRET")
val BMWtqw_cw_CmpLossLimExt_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_cw_CmpLossLimExt_C")
val A_quer_plaus: InMeasurement = a2lBin.measurement("A_quer_plaus")
val Md_rad_dynverl: InMeasurement = a2lBin.measurement("Md_rad_dynverl")
val Md_rad_ges_max_fd: InMeasurement = a2lBin.measurement("Md_rad_ges_max_fd")
val Md_rad_ges_min_fd: InMeasurement = a2lBin.measurement("Md_rad_ges_min_fd")
val Md_rad_max: InMeasurement = a2lBin.measurement("Md_rad_max")
val Md_rad_pedal: InMeasurement = a2lBin.measurement("Md_rad_pedal")
val Md_rad_soll_dcc: InMeasurement = a2lBin.measurement("Md_rad_soll_dcc")
val Md_rad_soll_sld: InMeasurement = a2lBin.measurement("Md_rad_soll_sld")
val Pwg_ist: InMeasurement = a2lBin.measurement("Pwg_ist")
val Pwg_ist_mafw: InMeasurement = a2lBin.measurement("Pwg_ist_mafw")
val St_anman: InMeasurement = a2lBin.measurement("St_anman")
val St_anman_B_antrieb: InMeasurement = a2lBin.measurement("St_anman.B_antrieb")
val St_anman_B_getreten: InMeasurement = a2lBin.measurement("St_anman.B_getreten")
val St_anman_B_kein_gang: InMeasurement = a2lBin.measurement("St_anman.B_kein_gang")
val St_anman_B_kupp_int: InMeasurement = a2lBin.measurement("St_anman.B_kupp_int")
val St_antrieb_soll: InMeasurement = a2lBin.measurement("St_antrieb_soll")
val St_mdrdmk: InMeasurement = a2lBin.measurement("St_mdrdmk")
val St_mdrdmk_B_fas_dcc: InMeasurement = a2lBin.measurement("St_mdrdmk.B_fas_dcc")
val St_mdrdmk_B_fas_sld: InMeasurement = a2lBin.measurement("St_mdrdmk.B_fas_sld")
val Status_antrieb_ist: InMeasurement = a2lBin.measurement("Status_antrieb_ist")
val W_radlenk: InMeasurement = a2lBin.measurement("W_radlenk")
val Md_rad_wunsch_fas: OutMeasurement = a2lBin.measurement("Md_rad_wunsch_fas")
val Md_rad_wunsch_grad: OutMeasurement = a2lBin.measurement("Md_rad_wunsch_grad")
val Md_rad_wunsch_vb: OutMeasurement = a2lBin.measurement("Md_rad_wunsch_vb")
val St_fw: OutMeasurement = a2lBin.measurement("St_fw")
val St_fw_B_hschalt_komb: OutMeasurement = a2lBin.measurement("St_fw.B_hschalt_komb")
val St_fw_B_ldm_akt04: OutMeasurement = a2lBin.measurement("St_fw.B_ldm_akt04")
val St_fw_B_ldm_off: OutMeasurement = a2lBin.measurement("St_fw.B_ldm_off")
val St_fw_B_ldm_offhs: OutMeasurement = a2lBin.measurement("St_fw.B_ldm_offhs")
val St_fw_B_ldm_offreg: OutMeasurement = a2lBin.measurement("St_fw.B_ldm_offreg")
val St_fw_B_ldm_offrs: OutMeasurement = a2lBin.measurement("St_fw.B_ldm_offrs")
val St_fw_B_rschalt_komb: OutMeasurement = a2lBin.measurement("St_fw.B_rschalt_komb")
val St_mdfw: OutMeasurement = a2lBin.measurement("St_mdfw")
val St_mdfw_B_sld_akt: OutMeasurement = a2lBin.measurement("St_mdfw.B_sld_akt")
val St_mdinfo_maw: OutMeasurement = a2lBin.measurement("St_mdinfo_maw")
val St_mdinfo_maw_B_fd_max_akt: OutMeasurement = a2lBin.measurement("St_mdinfo_maw.B_fd_max_akt")
val St_mdinfo_maw_B_fd_min_akt: OutMeasurement = a2lBin.measurement("St_mdinfo_maw.B_fd_min_akt")

  P_MAW_10ms(A_quer_plaus, Md_rad_dynverl, Md_rad_ges_max_fd, Md_rad_ges_min_fd, Md_rad_max, Md_rad_pedal, Md_rad_soll_dcc, Md_rad_soll_sld, Pwg_ist, Pwg_ist_mafw, St_anman, St_anman_B_antrieb, St_anman_B_getreten, St_anman_B_kein_gang, St_anman_B_kupp_int, St_antrieb_soll, St_mdrdmk, St_mdrdmk_B_fas_dcc, St_mdrdmk_B_fas_sld, Status_antrieb_ist, W_radlenk, KF_FAK_FGA_LIM_VM_START, KL_MANU_MD_RAD_MIN_FD, K_DELAY_MD_RAD_WUNSCH_GRAD, K_DT_MAX_FGA_VMSTART, K_F_FADE_FGA_INTERPRET_DN, K_F_FADE_FGA_INTERPRET_UP, K_LDM_PWGMAX, K_MAW_PWG_SLD_AUSSTIEG, K_MDIPM_HYS, K_MD_RAD_FIGA_MIN_STBY, K_MD_RAD_GES_FGA_MIN, K_TD_MAW_PWG_SLD_AUSSTIEG, MD_RAD_GES_MAX_FD_C, MD_RAD_GES_MAX_FD_V, MD_RAD_GES_MIN_FD_C, MD_RAD_GES_MIN_FD_V, S_MAW_SLD_AUSSTIEG_RAD_MAX, S_USE_AXHY_FGA_INTERPRET, BMWtqw_cw_CmpLossLimExt_C, Md_rad_wunsch_fas, Md_rad_wunsch_grad, Md_rad_wunsch_vb, St_fw, St_fw_B_hschalt_komb, St_fw_B_ldm_akt04, St_fw_B_ldm_off, St_fw_B_ldm_offhs, St_fw_B_ldm_offreg, St_fw_B_ldm_offrs, St_fw_B_rschalt_komb, St_mdfw, St_mdfw_B_sld_akt, St_mdinfo_maw, St_mdinfo_maw_B_fd_max_akt, St_mdinfo_maw_B_fd_min_akt)
}


def P_MXXLIM_10ms(BMWtqw_tqw_AxcHaLdcSpFild_sw: InMeasurement, BMWtqw_tqw_AxcHaLpaSpFild_sw: InMeasurement, BMWtqw_tqw_AxcHaSpFild_sw: InMeasurement, BMWtqw_tqw_AxcHaSpUnf_sw: InMeasurement, BMWtqw_tqw_DtHaMaxDyn_sw: InMeasurement, BMWtqw_tqw_DtHaMaxStat_sw: InMeasurement, BMWtqw_tqw_DtHaMinDyn_sw: InMeasurement, BMWtqw_tqw_DtHaMinStat_sw: InMeasurement, BMWtqw_tqw_StatLossHa_sw: InMeasurement, BMWtqw_tqw_VsbHaMaxDyn_sw: InMeasurement, BMWtqw_tqw_VsbHaMaxStat_sw: InMeasurement, BMWtqw_tqw_VsbHaMinDyn_sw: InMeasurement, BMWtqw_tqw_VsbHaMinStat_sw: InMeasurement, Status_md_ha: InMeasurement, BMWtqw_swi_DetIntvVsb_C: BigDecimal, S_DSC_EINRECH_TQW: String, BMWtqw_tqw_VsbHaLdcSpFild_sw: OutMeasurement, BMWtqw_tqw_VsbHaLpaSpFild_sw: OutMeasurement, BMWtqw_tqw_VsbHaSpFild_sw: OutMeasurement, BMWtqw_tqw_VsbHaSpUnf_sw: OutMeasurement, Md_rad_ha_fzgstab_max_dyn: OutMeasurement, Md_rad_ha_fzgstab_max_stat: OutMeasurement, Md_rad_ha_fzgstab_min_dyn: OutMeasurement, Md_rad_ha_fzgstab_min_stat: OutMeasurement, St_mxxlim_1: OutMeasurement, St_mxxlim_1_B_asr_mxxlim: OutMeasurement, St_mxxlim_1_B_msr_mxxlim: OutMeasurement, Status_md_gb1: OutMeasurement): Unit = {
 ???
}

def P_MXXLIM_10ms(a2lBin: A2LBinAdapter): Unit = {

val BMWtqw_swi_DetIntvVsb_C: BigDecimal = a2lBin.readCharacteristicWithCast("BMWtqw_swi_DetIntvVsb_C")
val S_DSC_EINRECH_TQW: String = a2lBin.readCharacteristicWithCast("S_DSC_EINRECH_TQW")
val BMWtqw_tqw_AxcHaLdcSpFild_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcHaLdcSpFild_sw")
val BMWtqw_tqw_AxcHaLpaSpFild_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcHaLpaSpFild_sw")
val BMWtqw_tqw_AxcHaSpFild_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcHaSpFild_sw")
val BMWtqw_tqw_AxcHaSpUnf_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_AxcHaSpUnf_sw")
val BMWtqw_tqw_DtHaMaxDyn_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_DtHaMaxDyn_sw")
val BMWtqw_tqw_DtHaMaxStat_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_DtHaMaxStat_sw")
val BMWtqw_tqw_DtHaMinDyn_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_DtHaMinDyn_sw")
val BMWtqw_tqw_DtHaMinStat_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_DtHaMinStat_sw")
val BMWtqw_tqw_StatLossHa_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_StatLossHa_sw")
val BMWtqw_tqw_VsbHaMaxDyn_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_VsbHaMaxDyn_sw")
val BMWtqw_tqw_VsbHaMaxStat_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_VsbHaMaxStat_sw")
val BMWtqw_tqw_VsbHaMinDyn_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_VsbHaMinDyn_sw")
val BMWtqw_tqw_VsbHaMinStat_sw: InMeasurement = a2lBin.measurement("BMWtqw_tqw_VsbHaMinStat_sw")
val Status_md_ha: InMeasurement = a2lBin.measurement("Status_md_ha")
val BMWtqw_tqw_VsbHaLdcSpFild_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_VsbHaLdcSpFild_sw")
val BMWtqw_tqw_VsbHaLpaSpFild_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_VsbHaLpaSpFild_sw")
val BMWtqw_tqw_VsbHaSpFild_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_VsbHaSpFild_sw")
val BMWtqw_tqw_VsbHaSpUnf_sw: OutMeasurement = a2lBin.measurement("BMWtqw_tqw_VsbHaSpUnf_sw")
val Md_rad_ha_fzgstab_max_dyn: OutMeasurement = a2lBin.measurement("Md_rad_ha_fzgstab_max_dyn")
val Md_rad_ha_fzgstab_max_stat: OutMeasurement = a2lBin.measurement("Md_rad_ha_fzgstab_max_stat")
val Md_rad_ha_fzgstab_min_dyn: OutMeasurement = a2lBin.measurement("Md_rad_ha_fzgstab_min_dyn")
val Md_rad_ha_fzgstab_min_stat: OutMeasurement = a2lBin.measurement("Md_rad_ha_fzgstab_min_stat")
val St_mxxlim_1: OutMeasurement = a2lBin.measurement("St_mxxlim_1")
val St_mxxlim_1_B_asr_mxxlim: OutMeasurement = a2lBin.measurement("St_mxxlim_1.B_asr_mxxlim")
val St_mxxlim_1_B_msr_mxxlim: OutMeasurement = a2lBin.measurement("St_mxxlim_1.B_msr_mxxlim")
val Status_md_gb1: OutMeasurement = a2lBin.measurement("Status_md_gb1")

  P_MXXLIM_10ms(BMWtqw_tqw_AxcHaLdcSpFild_sw, BMWtqw_tqw_AxcHaLpaSpFild_sw, BMWtqw_tqw_AxcHaSpFild_sw, BMWtqw_tqw_AxcHaSpUnf_sw, BMWtqw_tqw_DtHaMaxDyn_sw, BMWtqw_tqw_DtHaMaxStat_sw, BMWtqw_tqw_DtHaMinDyn_sw, BMWtqw_tqw_DtHaMinStat_sw, BMWtqw_tqw_StatLossHa_sw, BMWtqw_tqw_VsbHaMaxDyn_sw, BMWtqw_tqw_VsbHaMaxStat_sw, BMWtqw_tqw_VsbHaMinDyn_sw, BMWtqw_tqw_VsbHaMinStat_sw, Status_md_ha, BMWtqw_swi_DetIntvVsb_C, S_DSC_EINRECH_TQW, BMWtqw_tqw_VsbHaLdcSpFild_sw, BMWtqw_tqw_VsbHaLpaSpFild_sw, BMWtqw_tqw_VsbHaSpFild_sw, BMWtqw_tqw_VsbHaSpUnf_sw, Md_rad_ha_fzgstab_max_dyn, Md_rad_ha_fzgstab_max_stat, Md_rad_ha_fzgstab_min_dyn, Md_rad_ha_fzgstab_min_stat, St_mxxlim_1, St_mxxlim_1_B_asr_mxxlim, St_mxxlim_1_B_msr_mxxlim, Status_md_gb1)
}

}
