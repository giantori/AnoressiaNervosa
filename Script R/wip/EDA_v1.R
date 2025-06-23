# Per avere i titoli sulla dx -> ctrl + shift + o 

## ANORESSIA NERVOSA
# si vuole valutare se è possibile individuare i segni precoci di disfunzione
# articolare o muscolare caratteristici delle fasi iniziali di anoressia nervosa
# in strutture che non consentono le misurazioni di forza muscolare e funzionalità
# muscolare, e utilizzando al posto delle misurazioni ottenute dai test di
# forza muscolare (dinamometria), e dai test di performance fisica o funzionalità
# motoria (chair test, squat test, sit up test, siyt and reach test) i valori
# di altri parametri (parametri bioimpedenziometrici) come il phase angle, ECXW/ICW,
# BCM
# Bisognerà quindi valutare l'associazione tra i test di performance fisica che
# permettono di individuare gli stati preocici di anoressia nervosa
# e i parametri biodeimpedenziometrici, al fine di capire se è possibile utilizzare
# questi ultimi al posto dei primi

# Caricamento dati -----
rm(list = ls())
library(dplyr)
library(readxl)
dati <- read_excel("../../../Dati/input/dati_18062025_AN.xlsx", sheet = "dati T0-1-2", skip = 1, n_max = 25)
View(dati)

library(janitor)
dati = clean_names(dati) 
colnames(dati)

unique(dati$patologia)
table(dati$patologia)

# Rimozione di variabili  -----
# Rimozione colonna ID 
n_distinct(dati$paziente) == nrow(dati)
dati$paziente = NULL

# Rimozione di variabili costanti o nulle
n_unici = apply(dati, 2, n_distinct)
var_costanti = names(which(n_unici == 1))
# non ci sono variabili costanti o nulle

# Rimozione colonne data
# I tre tempi di misurazione sono a distanza di 2 mesi 
# ogni 2 mesi è stata effettuata la misurazione 
dati = dati %>% select(-data_ricovero)
dati = dati %>% select(-data_10)
dati = dati %>% select(-data_49)
dati = dati %>% select(-data_87)

# Rimozione colonna altezza2
dati = dati %>% select(-h2)

# Rimozione colonne FFM e FFMp
dati = dati %>% select(-ffm_kg, -ffm_kg_36, -ffm_kg_74, 
                        -ffm_percent_37, -ffm_percent_75, -ffm_percent_113)



# Modifiche sui nomi delle variabili ----
# più pratici da utilizzare

# variabili dalla 10 alla 48 riferite a T0
# variabili dalla 49 alle 86 riferite a T1
# variabili dalla 87 alle 123 riferite a T2

# individuaimo le variabili con sintassi simile nel nome (riferite ai tre
# diversi istanti temporali)
var_simili = grep("peso", names(dati), val = T)
var_simili
dati = dati %>% rename(peso_T0 = peso_kg_11)       
dati = dati %>% rename(peso_T1 = peso)       
dati = dati %>% rename(peso_T2 = peso_kg_50)

var_simili = grep("bmi", names(dati), val = T)
var_simili
dati = dati %>% rename(bmi_T0 = bmi_kg_m_2_12)       
dati = dati %>% rename(bmi_T1 = bmi_kg_m_2_51)       
dati = dati %>% rename(bmi_T2 = bmi_kg_m_2_89)


# sx 1, sx 2, sx 3, dx 1, dx 2, dx 3, 
# Prima, seconda e terza prova di massima contrazione volontaria;
# si eseguono tipicamente tre tentativi per mano per ridurre la 
# variabilità intra-soggetto.

# Viene considerate anche la media e la deviazione standard, rispettivamente
# per avere un indice riassuntivo della forza massima dell’arto
# e per valutare la coerenza degli sforzi: una DS alta indica prove poco 
# ripetibili (scarso impegno, dolore, fatica, ecc.).

var_simili = grep("sx", names(dati), val = T)
var_simili
dati = dati %>% rename(sx1_T0 = sx_1_kg_14)       
dati = dati %>% rename(sx2_T0 = sx_2_kg)       
dati = dati %>% rename(sx3_T0 = sx_3_kg_16)       
dati = dati %>% rename(sx1_T1 = sx_1_kg_52)       
dati = dati %>% rename(sx2_T1 = sx_2_kg_53)       
dati = dati %>% rename(sx3_T1 = sx_3_kg_54)       
dati = dati %>% rename(sx1_T2 = sx_1_kg_90)       
dati = dati %>% rename(sx2_T2 = sx_2_kg_91)       
dati = dati %>% rename(sx3_T2 = sx_3_kg_92)   

var_simili = grep("dx", names(dati), val = T)
var_simili
dati = dati %>% rename(dx1_T0 = dx_1_kg_19)       
dati = dati %>% rename(dx2_T0 = dx_2_kg_20)       
dati = dati %>% rename(dx3_T0 = dx_3_kg_21)       
dati = dati %>% rename(dx1_T1 = dx_1_kg_57)       
dati = dati %>% rename(dx2_T1 = dx_2_kg_58)       
dati = dati %>% rename(dx3_T1 = dx_3_kg_59)       
dati = dati %>% rename(dx1_T2 = dx_1_kg_95)       
dati = dati %>% rename(dx2_T2 = dx_2_kg)       
dati = dati %>% rename(dx3_T2 = dx_3_kg_97)   

var_simili = grep("media", names(dati), val = T)
var_simili
dati = dati %>% rename(mediasx_T0 = media_sx_kg_17)       
dati = dati %>% rename(mediasx_T1 = media_sx_kg_55)       
dati = dati %>% rename(mediasx_T2 = media_sx_kg_93)
dati = dati %>% rename(mediadx_T0 = media_dx_kg_22)       
dati = dati %>% rename(mediadx_T1 = media_dx_kg_60)       
dati = dati %>% rename(mediadx_T2 = media_dx_kg_98)

var_simili = grep("d_s", names(dati), val = T)
var_simili
dati = dati %>% rename(dssx_T0 = d_s_sx)       
dati = dati %>% rename(dssx_T1 = d_s_sx_2)       
dati = dati %>% rename(dssx_T2 = d_s_sx_kg)
dati = dati %>% rename(dsdx_T0 = d_s_dx)       
dati = dati %>% rename(dsdx_T1 = d_s_dx_2)       
dati = dati %>% rename(dsdx_T2 = d_s_dx_kg)

var_simili = grep("sit", names(dati), val = T)
var_simili
dati = dati %>% rename(situptest_T0 = sit_up_test)       
dati = dati %>% rename(situptest_T1 = sit_up_test_2)       
dati = dati %>% rename(situptest_T2 = sit_up_test_3)       

var_simili = grep("squat", names(dati), val = T)
var_simili
dati = dati %>% rename(squattest_T0 = squat_test)       
dati = dati %>% rename(squattest_T1 = squat_test_63)       
dati = dati %>% rename(squattest_T2 = squat_test_101)       

var_simili = grep("chair", names(dati), val = T)
var_simili
dati = dati %>% rename(chairstandtest_T0 = chair_stand_test)       
dati = dati %>% rename(chairstandtest_T1 = chair_test)       
dati = dati %>% rename(chairstandtest_T2 = chair_test_2)     

var_simili = grep("s_r_", names(dati), val = T)
var_simili
dati = dati %>% rename(sitreachtest_T0 = s_r_cm)       
dati = dati %>% rename(sitreachtest_T1 = s_r_test_cm)       
dati = dati %>% rename(sitreachtest_T2 = s_r_test_cm_2)     

var_simili = grep("kcal", names(dati), val = T)
var_simili
dati = dati %>% rename(mkcal_T0 = media_kcal_assunte_10gg_pre)       
dati = dati %>% rename(mkcal_T1 = media_kcal_assunte_10g_pre_66)       
dati = dati %>% rename(mkcal_T2 = media_kcal_assunte_10g_pre_104)     

var_simili = grep("rmr", names(dati), val = T)
var_simili
dati = dati %>% rename(RMR_T0 = rmr_kcal_die_29)       
dati = dati %>% rename(RMR_T1 = rmr_kcal_die_67)       
dati = dati %>% rename(RMR_T2 = rmr_kcal_die_105)   

var_simili = grep("vo", names(dati), val = T)
var_simili
dati = dati %>% rename(VO2_T0 = vo2_ml_min_kg)       
dati = dati %>% rename(VO2_T1 = vo2_ml_min_kg_68)       
dati = dati %>% rename(VO2_T2 = vo2_ml_min_kg_106) 

var_simili = grep("rq", names(dati), val = T)
var_simili
dati = dati %>% rename(RQ_T0 = rq_31)       
dati = dati %>% rename(RQ_T1 = rq_69)       
dati = dati %>% rename(RQ_T2 = rq_107) 

var_simili = grep("rx", names(dati), val = T)
var_simili
dati = dati %>% rename(rx_T0 = rx_ohm_32)       
dati = dati %>% rename(rx_T1 = rx_ohm_70)       
dati = dati %>% rename(rx_T2 = rx_ohm_108) 

var_simili = grep("xc", names(dati), val = T)
var_simili
dati = dati %>% rename(xc_T0 = xc_ohm_33)       
dati = dati %>% rename(xc_T1 = xc_ohm_71)       
dati = dati %>% rename(xc_T2 = xc_ohm_109) 

var_simili = grep("fm", names(dati), val = T)
var_simili
dati = dati %>% rename(FM_T0 = fm_kg_34)       
dati = dati %>% rename(FM_T1 = fm_kg_72)       
dati = dati %>% rename(FM_T2 = fm_kg_110) 

dati = dati %>% rename(FMp_T0 = fm_percent_35)       
dati = dati %>% rename(FMp_T1 = fm_percent_73)       
dati = dati %>% rename(FMp_T2 = fm_percent) 

dati = dati %>% rename(FFMI_T0 = ffmi_t0)       
dati = dati %>% rename(FFMI_T1 = ffmi_t1)       
dati = dati %>% rename(FFMI_T2 = ffmi_t2) 

var_simili = grep("tbw", names(dati), val = T)
var_simili
dati = dati %>% rename(TBW_T0 = tbw_l_39)       
dati = dati %>% rename(TBW_T1 = tbw_l_77)       
dati = dati %>% rename(TBW_T2 = tbw) 

var_simili = grep("tbw", names(dati), val = T)
var_simili
dati = dati %>% rename(TBWp_T0 = tbw_percent_40)       
dati = dati %>% rename(TBWp_T1 = tbw_percent_78)       
dati = dati %>% rename(TBWp_T2 = tbw_percent_116) 

var_simili = grep("ecw", names(dati), val = T)
var_simili
dati = dati %>% rename(ECW_T0 = ecw_l_41)       
dati = dati %>% rename(ECW_T1 = ecw_l_79)       
dati = dati %>% rename(ECW_T2 = ecw) 

dati = dati %>% rename(ECWp_T0 = ecw_percent_42)       
dati = dati %>% rename(ECWp_T1 = ecw_percent_80)       
dati = dati %>% rename(ECWp_T2 = ecw_percent_118) 

var_simili = grep("icw", names(dati), val = T)
var_simili
dati = dati %>% rename(ICW_T0 = icw_l_43)       
dati = dati %>% rename(ICW_T1 = icw_l_81)       
dati = dati %>% rename(ICW_T2 = icw) 

dati = dati %>% rename(ICWp_T0 = icw_percent_44)       
dati = dati %>% rename(ICWp_T1 = icw_percent_82)       
dati = dati %>% rename(ICWp_T2 = icw_percent_120)

var_simili = grep("bcm", names(dati), val = T)
var_simili
dati = dati %>% rename(BCM_T0 = bcm_kg_45)       
dati = dati %>% rename(BCM_T1 = bcm_kg_83)       
dati = dati %>% rename(BCM = bcm_kg_121)

dati = dati %>% rename(BCMI_T0 = bcmi_kg_m_47)       
dati = dati %>% rename(BCMI_T1 = bcmi_kg_m_85)       
dati = dati %>% rename(BCMI = bcmi_kg_m) 

var_simili = grep("pa", names(dati), val = T)
var_simili
dati = dati %>% rename(pha_T0 = pa_46)       
dati = dati %>% rename(pha_T1 = pa_84)       
dati = dati %>% rename(pha_T2 = pa_122)


# Correzione degli errori di scrittura/ ricodifica modalità variabili -----
apply(dati, 2, n_distinct)
str(dati$media_kcal_assunte_10gg_pre)
dati$media_kcal_assunte_10gg_pre <-
  gsub("1479 \\(avanza quasi sempre i carboidrati a colazione\\)",
       "1479",
       dati$media_kcal_assunte_10gg_pre)

# Valori mancanti -----
sum(is.na(dati))  
library(DataExplorer)
plot_missing(dati)
na_fun = function(dati){
  na = sapply(dati, function(x) sum(is.na(x)))
  na = sort(na[na > 0])
  na = data.frame(
    variabile = names(na),
    freq_assoluta = as.numeric(na),
    freq_relativa = round(as.numeric(na)/nrow(dati), 4)
  )
  na
}
na = na_fun(dati)
na

# dataset con le variabili con NA
dati_na = dati %>% select(all_of(var_na))


# Congruenza tra le osservazioni con NA tra le variabili
var_na = na$variabile
var_na
str(var_na)
congruent_missing <- complete.cases(dati %>% select(all_of(var_na)))
unique(congruent_missing) 
# se sono tutti TRUE allora le variabili con dati mancanti hanno le stesse oss con dati mancanti
# se ci sono anche FALSE, vedere quanti ce ne sono, per poter fare un'eliminazione sempre con criterio 
oss_na <- which(!congruent_missing)  # oss che differiscono
oss_na
length(oss_na)  # vediamo quante sono quelle che differiscono


# vediamo prima quante sono le oss che hanno valori mancanti per quella variabile
length(dati$sintomo_2[is.na(dati$sintomo_2)])
# successivamente definiamo tutti i valori mancanti come "Valori mancanti"
# deve essere character la variabile!
dati$sintomo_2 = as.character(dati$sintomo_2)
table(dati$sintomo_2)
dati$sintomo_2[is.na(dati$sintomo_2)] = "nessun sintomo"
# oppure con "altro"






