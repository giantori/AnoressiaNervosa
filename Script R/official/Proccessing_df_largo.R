library(dplyr)
library(readxl)
library(janitor)
library(DataExplorer)
library(writexl)

# Caricamento dati
dati <- read_excel("../../../Dati/input/dati_18062025_AN.xlsx", sheet = "dati T0-1-2", skip = 1, n_max = 25)
dati <- clean_names(dati) 

# Rimozione colonne
dati$paziente <- NULL

dati <- dati %>% select(-data_ricovero, -data_10, -data_49, -data_87, -h2)
dati <- dati %>% select(-ffm_kg, -ffm_kg_36, -ffm_kg_74, -ffm_percent_37, -ffm_percent_75, -ffm_percent_113)

# Rinomina variabili
dati <- dati %>% rename(
  peso_T0 = peso_kg_11,
  peso_T1 = peso,
  peso_T2 = peso_kg_50,
  
  bmi_T0 = bmi_kg_m_2_12,
  bmi_T1 = bmi_kg_m_2_51,
  bmi_T2 = bmi_kg_m_2_89,
  
  sx1_T0 = sx_1_kg_14,
  sx2_T0 = sx_2_kg,
  sx3_T0 = sx_3_kg_16,
  sx1_T1 = sx_1_kg_52,
  sx2_T1 = sx_2_kg_53,
  sx3_T1 = sx_3_kg_54,
  sx1_T2 = sx_1_kg_90,
  sx2_T2 = sx_2_kg_91,
  sx3_T2 = sx_3_kg_92,
  
  dx1_T0 = dx_1_kg_19,
  dx2_T0 = dx_2_kg_20,
  dx3_T0 = dx_3_kg_21,
  dx1_T1 = dx_1_kg_57,
  dx2_T1 = dx_2_kg_58,
  dx3_T1 = dx_3_kg_59,
  dx1_T2 = dx_1_kg_95,
  dx2_T2 = dx_2_kg,
  dx3_T2 = dx_3_kg_97,
  
  mediasx_T0 = media_sx_kg_17,
  mediasx_T1 = media_sx_kg_55,
  mediasx_T2 = media_sx_kg_93,
  mediadx_T0 = media_dx_kg_22,
  mediadx_T1 = media_dx_kg_60,
  mediadx_T2 = media_dx_kg_98,
  
  dssx_T0 = d_s_sx,
  dssx_T1 = d_s_sx_2,
  dssx_T2 = d_s_sx_kg,
  dsdx_T0 = d_s_dx,
  dsdx_T1 = d_s_dx_2,
  dsdx_T2 = d_s_dx_kg,
  
  situptest_T0 = sit_up_test,
  situptest_T1 = sit_up_test_2,
  situptest_T2 = sit_up_test_3,
  
  squattest_T0 = squat_test,
  squattest_T1 = squat_test_63,
  squattest_T2 = squat_test_101,
  
  chairstandtest_T0 = chair_stand_test,
  chairstandtest_T1 = chair_test,
  chairstandtest_T2 = chair_test_2,
  
  sitreachtest_T0 = s_r_cm,
  sitreachtest_T1 = s_r_test_cm,
  sitreachtest_T2 = s_r_test_cm_2,
  
  mkcal_T0 = media_kcal_assunte_10gg_pre,
  mkcal_T1 = media_kcal_assunte_10g_pre_66,
  mkcal_T2 = media_kcal_assunte_10g_pre_104,
  
  RMR_T0 = rmr_kcal_die_29,
  RMR_T1 = rmr_kcal_die_67,
  RMR_T2 = rmr_kcal_die_105,
  
  VO2_T0 = vo2_ml_min_kg,
  VO2_T1 = vo2_ml_min_kg_68,
  VO2_T2 = vo2_ml_min_kg_106,
  
  RQ_T0 = rq_31,
  RQ_T1 = rq_69,
  RQ_T2 = rq_107,
  
  rx_T0 = rx_ohm_32,
  rx_T1 = rx_ohm_70,
  rx_T2 = rx_ohm_108,
  
  xc_T0 = xc_ohm_33,
  xc_T1 = xc_ohm_71,
  xc_T2 = xc_ohm_109,
  
  FM_T0 = fm_kg_34,
  FM_T1 = fm_kg_72,
  FM_T2 = fm_kg_110,
  
  FMp_T0 = fm_percent_35,
  FMp_T1 = fm_percent_73,
  FMp_T2 = fm_percent,
  
  FFMI_T0 = ffmi_t0,
  FFMI_T1 = ffmi_t1,
  FFMI_T2 = ffmi_t2,
  
  TBW_T0 = tbw_l_39,
  TBW_T1 = tbw_l_77,
  TBW_T2 = tbw,
  
  TBWp_T0 = tbw_percent_40,
  TBWp_T1 = tbw_percent_78,
  TBWp_T2 = tbw_percent_116,
  
  ECW_T0 = ecw_l_41,
  ECW_T1 = ecw_l_79,
  ECW_T2 = ecw,
  
  ECWp_T0 = ecw_percent_42,
  ECWp_T1 = ecw_percent_80,
  ECWp_T2 = ecw_percent_118,
  
  ICW_T0 = icw_l_43,
  ICW_T1 = icw_l_81,
  ICW_T2 = icw,
  
  ICWp_T0 = icw_percent_44,
  ICWp_T1 = icw_percent_82,
  ICWp_T2 = icw_percent_120,
  
  BCM_T0 = bcm_kg_45,
  BCM_T1 = bcm_kg_83,
  BCM = bcm_kg_121,
  
  BCMI_T0 = bcmi_kg_m_47,
  BCMI_T1 = bcmi_kg_m_85,
  BCMI = bcmi_kg_m,
  
  pha_T0 = pa_46,
  pha_T1 = pa_84,
  pha_T2 = pa_122
)

# Pulizia valori non numerici in variabili numeriche
dati$mkcal_T0 <- gsub("^(\\d+).*", "\\1", dati$mkcal_T0)

# Pulizia variazione_menu "si", "no"
dati <- dati %>% mutate(across(contains("variazione_menu"), ~ ifelse(. == "no", "no", "si")))

# Gestione NA su sintomo_2
dati$sintomo_2 <- as.character(dati$sintomo_2)
dati$sintomo_2[is.na(dati$sintomo_2)] <- "nessun sintomo"

write_xlsx(dati, path = "../../../Dati/output/dati_larghi_18062025_AN.xlsx")

