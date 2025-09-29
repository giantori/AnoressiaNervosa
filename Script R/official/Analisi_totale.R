##############################################
# PROGETTO: Sostituibilità predittiva B -> A #
# Autore:    (tuo nome)                      #
# Data:      2025-08-27                      #
##############################################

# =========================
# 0) Librerie
# =========================
library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(broom)          # per tidying modelli lm/lmer
library(broom.mixed)    # per lmer
library(MuMIn)          # R2 per mixed models
library(patchwork)      # combinare grafici ggplot
library(scales)         # utilità per etichette

# (Opzionali per esplorativo multiblocco; scommenta se vuoi usarli)
# library(FactoMineR)   # MFA + RV
# library(ade4)         # RV.rtest
# library(CCA)          # CCA

# =========================
# 1) Percorsi file
# =========================
path_X  <- "output/dati_lunghi_2025-08-26_AN.xlsx"
path_A  <- "dati_long_mc.xlsx"
path_B  <- "dati_long_ff.xlsx"

# =========================
# 2) Lettura dati
# =========================
X_raw <- read_excel(path_X)
A_raw <- read_excel(path_A)
B_raw <- read_excel(path_B)

# Controllo colonne chiave
stopifnot(all(c("paziente","tempo") %in% names(X_raw)))
stopifnot(all(c("paziente","tempo") %in% names(A_raw)))
stopifnot(all(c("paziente","tempo") %in% names(B_raw)))


# =========================
# 3) Definizione variabili
# =========================
# X (covariate) come da tua lista (lasciare tra backticks se hanno spazi)
X_vars <- c("patologia", "sintomo 1", "sintomo 2",
            "arto dominante", "altezza_m", "sesso",
            "peso", "bmi", "mkcal", "variazionemenu")

# GRUPPO A (KPI di riferimento, costosi) — 17 variabili
A_vars <- c("RMR","VO2","RQ","rx","xc","FM","FMp","FFMI",
            "TBW","TBWp","ECW","ECWp","ICW","ICWp","BCM","pha","BCMI")

# GRUPPO B (KPI economici) — 14 variabili
B_vars <- c("sx1","sx2","sx3","dx1","dx2","dx3",
            "mediasx","mediadx","dssx","dsdx",
            "situptest","squattest","chairstandtest","sitreachtest")

# =========================
# 4) Pre-processing
# =========================

# 4.1) Se alcune X hanno spazi, creo anche alias "snake_case" per comodità nei grafici,
#      ma nelle formule userò backticks per sicurezza.
#      (Questo passaggio è opzionale: non cambia i nomi originali.)
make_alias <- function(nm) gsub("\\s+","_", nm)
X_alias <- setNames(make_alias(X_vars), X_vars)

# 4.2) Se manca bmi ma ho peso & altezza_m, lo calcolo; altrimenti preferisco usare bmi
X <- X_raw %>%
  mutate(
    tempo    = as.factor(tempo),
    across(all_of(intersect(c("sesso","patologia","arto dominante"), names(.))), as.factor)
  ) %>%
  mutate(
    bmi = case_when(
      "bmi" %in% names(.) ~ `bmi`,
      "peso" %in% names(.) & "altezza_m" %in% names(.) ~ `peso` / ((`altezza_m`)^2),
      TRUE ~ NA_real_
    )
  )


# 1) Se serve, uniformo il nome della chiave id
if (!"paziente" %in% names(X_raw) && "paziente" %in% names(X_raw)) X_raw <- dplyr::rename(X_raw, paziente = paziente)
if (!"paziente" %in% names(A_raw) && "paziente" %in% names(A_raw)) A_raw <- dplyr::rename(A_raw, paziente = paziente)
if (!"paziente" %in% names(B_raw) && "paziente" %in% names(B_raw)) B_raw <- dplyr::rename(B_raw, paziente = paziente)

# 2) Rendo 'tempo' dello stesso tipo in TUTTI i data frame prima del join
#    (uso 'character' per evitare conflitti factor/numeric)
X <- X_raw %>% dplyr::mutate(tempo = as.character(tempo))
A <- A_raw %>% dplyr::mutate(tempo = as.character(tempo))
B <- B_raw %>% dplyr::mutate(tempo = as.character(tempo))

# 3) (Opzionale ma utile) Controllo i valori distinti di 'tempo' per vedere se combaciano
unique(X$tempo); unique(A$tempo); unique(B$tempo)

# 4) Faccio il join con la chiave corretta (paziente, tempo)
df0 <- X %>%
  dplyr::select(paziente, tempo, dplyr::all_of(X_vars[X_vars %in% names(X)])) %>%
  dplyr::left_join(A, by = c("paziente","tempo")) %>%
  dplyr::left_join(B, by = c("paziente","tempo"))

# 5) Ripristino 'tempo' come fattore ordinato (se vuoi T1–T3 o 1–3)
#    Se i valori sono "1","2","3":
df0 <- df0 %>% dplyr::mutate(tempo = factor(tempo, levels = c("1","2","0")))

# 4.4) Standardizzazione per blocco e KPI z-mediato
#      Standardizzo OGNI variabile del blocco su tutto il dataset (non per tempo),
#      così KPI_A e KPI_B sono confrontabili nel modello con effetto fisso di 'tempo'.
z_scale <- function(x) as.numeric(scale(x))

df_kpi <- df0 %>%
  mutate(
    across(all_of(A_vars), z_scale, .names = "zA_{col}"),
    across(all_of(B_vars), z_scale, .names = "zB_{col}")
  ) %>%
  mutate(
    KPI_A = rowMeans(across(starts_with("zA_")), na.rm = TRUE),
    KPI_B = rowMeans(across(starts_with("zB_")), na.rm = TRUE)
  )

# 4.5) Pulizia X per collinearità: privilegio bmi (evito usare insieme peso+altezza_m+bmi)
#      Mantengo: patologia, sintomo 1, sintomo 2, arto dominante, sesso, bmi, mkcal, variazionemenu
X_keep <- c("patologia", "sintomo 1", "sintomo 2", "arto dominante", "sesso", "bmi", "mkcal", "variazionemenu")
X_keep <- X_keep[X_keep %in% names(df_kpi)]

# 4.6) Dataset finale
df <- df_kpi %>%
  select(paziente, tempo, all_of(X_keep), KPI_A, KPI_B)

# Salvataggio df
write.xlsx(df, file = "df.xlsx")


# =========================
# 5) DESCRITTIVO
# =========================

# 5.1) Mancanti rapidi
missing_tbl <- df %>%
  summarize(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variabile", values_to = "n_mancanti") %>%
  arrange(desc(n_mancanti))
print(missing_tbl)

# 5.2) Distribuzioni KPI per tempo
p_dens_A <- ggplot(df, aes(KPI_A, fill = tempo)) +
  geom_density(alpha = 0.35) +
  labs(title = "Distribuzione KPI_A per tempo", x = "KPI_A (z-media blocco A)", y = "Densità") +
  theme_minimal()

p_dens_B <- ggplot(df, aes(KPI_B, fill = tempo)) +
  geom_density(alpha = 0.35) +
  labs(title = "Distribuzione KPI_B per tempo", x = "KPI_B (z-media blocco B)", y = "Densità") +
  theme_minimal()

(p_dens_A | p_dens_B)

# 5.3) Relazione A vs B per tempo (scatter + retta)
p_scatter <- ggplot(df, aes(KPI_B, KPI_A, color = tempo)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relazione sincrona KPI_A vs KPI_B per tempo",
       x = "KPI_B (z-media blocco B)",
       y = "KPI_A (z-media blocco A)") +
  theme_minimal()
p_scatter

# Le variabili riferite ai marcatori (KPI_A) e le variabili riferite alla forza
# muscolare, funzionalità motoria (FPI_B) si muovono nella stessa direzione
# scatter con pendenza positiva) la relazione però appare moderata, la nuvola
# di punti è larga.

# assicurati che 'tempo' sia un fattore discreto ordinato (0-1-2 oppure 1-2-3 o T1-T2-T3)
levels_tempo <- sort(unique(as.character(df$tempo)))
df_plot <- df %>%
  mutate(tempo = factor(as.character(tempo), levels = levels_tempo))

# long + grouping corretto: UNA linea per ciascun (paziente, KPI)
df_longKPI <- df_plot %>%
  tidyr::pivot_longer(cols = c(KPI_A, KPI_B), names_to = "KPI", values_to = "valore")

p_spaghetti <- ggplot(df_longKPI,
                      aes(x = tempo, y = valore,
                          group = interaction(paziente, KPI),  # << CORREZIONE
                          color = KPI)) +
  geom_line(alpha = 0.35) +
  geom_point(alpha = 0.8, position = position_jitter(width = 0.03, height = 0)) +
  labs(title = "Andamento temporale KPI per soggetto",
       x = "Tempo", y = "Valore KPI (z)") +
  theme_minimal()

p_spaghetti

# Le densità e lo spaghetti plot mostrano maggiore dispersione/fluttuazione 
# per i KPI_B e questo è comunque coerente con misure funzionali più sensibili a
# fattori di giornata, motivazione, fatica, apprendimento)
# I KPI_B risultano più rumorosi dei KPI_A


# 5.5) Bland–Altman 'grezzo' per tempo (KPI_A vs KPI_B)
df_ba <- df %>%
  mutate(
    mean_AB = (KPI_A + KPI_B)/2,
    diff_AB = KPI_A - KPI_B
  )

p_ba <- ggplot(df_ba, aes(mean_AB, diff_AB, color = tempo)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = mean(df_ba$diff_AB, na.rm = TRUE), linetype = "dashed") +
  geom_hline(yintercept = mean(df_ba$diff_AB, na.rm = TRUE) + 1.96*sd(df_ba$diff_AB, na.rm = TRUE), linetype = "dotted") +
  geom_hline(yintercept = mean(df_ba$diff_AB, na.rm = TRUE) - 1.96*sd(df_ba$diff_AB, na.rm = TRUE), linetype = "dotted") +
  labs(title = "Bland–Altman grezzo tra KPI_A e KPI_B",
       x = "Media (A,B)", y = "Differenza (A - B)") +
  theme_minimal()
p_ba

# Cercare il signficato di Bland-Altman, capire come interpretarlo 

# (Opzionale) MFA/CCA per tempo su blocchi originali — utile per storytelling strutturale
# ESEMPIO (scommenta se vuoi provarlo, assicurati che non ci siano troppi NA):
# for (tt in levels(df0$tempo)) {
#   dtt <- df0 %>% filter(tempo == tt)
#   A_tt <- dtt %>% select(all_of(A_vars)) %>% scale() %>% as.data.frame()
#   B_tt <- dtt %>% select(all_of(B_vars)) %>% scale() %>% as.data.frame()
#   print(tt)
#   print(FactoMineR::RVcoef(A_tt, B_tt))   # coefficiente RV globale
#   # CCA semplice:
#   cca_fit <- CCA::cc(A_tt, B_tt)
#   print(cca_fit$cor)                      # correlazioni canoniche
# }

# =========================
# 6) INFERENZIALE
# =========================

# 6.1) MCID "distribution-based": 0.5 * SD_change(KPI_A)
#      Calcolo differenze entro paziente tra tempi adiacenti e stimo SD
df_change <- df %>%
  arrange(paziente, tempo) %>%
  group_by(paziente) %>%
  mutate(KPI_A_lead = lead(KPI_A)) %>%
  ungroup() %>%
  mutate(diff_A = KPI_A_lead - KPI_A) %>%
  filter(!is.na(diff_A))

SD_change_A <- sd(df_change$diff_A, na.rm = TRUE)
MCID <- 0.5 * SD_change_A
MCID

# 6.2) Modello misto sincrono: A ~ B + X + (1|paziente) + effetto fisso di tempo
#      Nota: uso bmi (evito peso+altezza_m insieme). Variabili categoriche restano fattori.
form_base <- as.formula(
  paste0("KPI_A ~ KPI_B + tempo + ",
         paste(sprintf("`%s`", X_keep[X_keep != "altezza_m" & X_keep != "peso"]), collapse = " + "),
         " + (1|paziente)")
)

# Fitting (usando REML; NA esclusi riga per riga)
mod_base <- lmer(form_base, data = df, REML = TRUE, na.action = na.omit)
summary(mod_base)
MuMIn::r.squaredGLMM(mod_base)

# 6.3) Predizioni e metriche su tutto il dataset (attenzione: non è CV)
df$A_hat_fixed <- predict(mod_base, newdata = df, re.form = ~0, allow.new.levels = TRUE)
df$A_resid     <- df$KPI_A - df$A_hat_fixed

overall_RMSE <- sqrt(mean((df$KPI_A - df$A_hat_fixed)^2, na.rm = TRUE))
overall_MAE  <- mean(abs(df$KPI_A - df$A_hat_fixed), na.rm = TRUE)
overall_RMSE; overall_MAE

# 6.4) Leave-One-Subject-Out CV (LOSO) con predizioni da soli effetti fissi
loso_cv <- function(data, formula) {
  ids <- unique(data$paziente)
  out <- vector("list", length(ids))
  for (k in seq_along(ids)) {
    id_test <- ids[k]
    train <- data %>% filter(paziente != id_test) %>% drop_na(all_of(all.vars(formula)))
    test  <- data %>% filter(paziente == id_test)
    
    if (nrow(train) < 10 || nrow(test) == 0) next
    
    fit <- try(lmer(formula, data = train, REML = TRUE, na.action = na.omit,
                    control = lmerControl(check.conv.singular = "ignore")), silent = TRUE)
    if (inherits(fit, "try-error")) next
    
    pred <- predict(fit, newdata = test, re.form = ~0, allow.new.levels = TRUE)
    out[[k]] <- tibble(paziente = id_test,
                       obs = test$KPI_A,
                       pred = pred)
  }
  bind_rows(out)
}

cv_pred <- loso_cv(df, form_base)
cv_metrics <- cv_pred %>%
  summarize(
    RMSE = sqrt(mean((obs - pred)^2, na.rm = TRUE)),
    MAE  = mean(abs(obs - pred), na.rm = TRUE),
    R2   = cor(obs, pred, use = "pairwise.complete.obs")^2
  )
cv_metrics

# 6.5) Calibrazione: z(A) ~ z(A_hat) — TOST su intercetta e slope
df_cal <- df %>%
  filter(!is.na(A_hat_fixed), !is.na(KPI_A)) %>%
  mutate(zA = as.numeric(scale(KPI_A)),
         zAhat = as.numeric(scale(A_hat_fixed)))

cal_fit <- lm(zA ~ zAhat, data = df_cal)
summary(cal_fit)

# Intervalli al 90% (TOST) per intercetta e slope
ci90 <- confint(cal_fit, level = 0.90)
ci90

# Equivalence bounds (proposti)
b_interc_lo <- -0.2; b_interc_hi <-  0.2   # in SD (Cohen's d piccolo)
b_slope_lo  <-  0.9; b_slope_hi  <-  1.1

equiv_interc <- (ci90["(Intercept)",1] >= b_interc_lo) & (ci90["(Intercept)",2] <= b_interc_hi)
equiv_slope  <- (ci90["zAhat",1]      >= b_slope_lo)  & (ci90["zAhat",2]      <= b_slope_hi)

equiv_interc; equiv_slope

# 6.6) Bland–Altman CONDIZIONALE (su residui 'tolte' le X)
# Modelli separati A~X e B~X con (1|paziente), poi BA tra residui
form_XA <- as.formula(
  paste0("KPI_A ~ tempo + ",
         paste(sprintf("`%s`", X_keep[X_keep != "altezza_m" & X_keep != "peso"]), collapse = " + "),
         " + (1|paziente)")
)
form_XB <- as.formula(
  paste0("KPI_B ~ tempo + ",
         paste(sprintf("`%s`", X_keep[X_keep != "altezza_m" & X_keep != "peso"]), collapse = " + "),
         " + (1|paziente)")
)

mod_AX <- lmer(form_XA, data = df, REML = TRUE, na.action = na.omit)
mod_BX <- lmer(form_XB, data = df, REML = TRUE, na.action = na.omit)

df$eA <- with(df, KPI_A - predict(mod_AX, newdata = df, re.form = ~0, allow.new.levels = TRUE))
df$eB <- with(df, KPI_B - predict(mod_BX, newdata = df, re.form = ~0, allow.new.levels = TRUE))

df_ba_cond <- df %>%
  transmute(paziente, tempo,
            mean_e = (eA + eB)/2,
            diff_e = (eA - eB))

ba_bias  <- mean(df_ba_cond$diff_e, na.rm = TRUE)
ba_sd    <- sd(df_ba_cond$diff_e, na.rm = TRUE)
ba_LoA_l <- ba_bias - 1.96*ba_sd
ba_LoA_u <- ba_bias + 1.96*ba_sd
c(ba_bias = ba_bias, LoA_low = ba_LoA_l, LoA_up = ba_LoA_u, MCID = MCID)

p_ba_cond <- ggplot(df_ba_cond, aes(mean_e, diff_e, color = tempo)) +
  geom_point(alpha = 0.8) +
  geom_hline(yintercept = ba_bias, linetype = "dashed") +
  geom_hline(yintercept = ba_LoA_l, linetype = "dotted") +
  geom_hline(yintercept = ba_LoA_u, linetype = "dotted") +
  geom_hline(yintercept =  MCID,    linetype = "longdash") +
  geom_hline(yintercept = -MCID,    linetype = "longdash") +
  labs(title = "Bland–Altman condizionale (residui A e B tolte le X)",
       x = "Media residui (A,B | X)", y = "Differenza residui (A - B | X)") +
  theme_minimal()
p_ba_cond

# 6.7) Condivisione del non-osservato: correlazione tra residui eA vs eB
corr_e <- cor(df$eA, df$eB, use = "pairwise.complete.obs")
corr_e

# Bootstrap clusterizzato (per IC della correlazione residua)
set.seed(123)
boot_corr <- function(data, B = 500) {
  ids <- unique(data$paziente)
  out <- numeric(B)
  for (b in seq_len(B)) {
    samp_ids <- sample(ids, size = length(ids), replace = TRUE)
    dd <- map_dfr(samp_ids, ~ filter(data, paziente == .x))
    out[b] <- suppressWarnings(cor(dd$eA, dd$eB, use = "pairwise.complete.obs"))
  }
  out
}
corr_boot <- boot_corr(df, B = 500)
quantile(corr_boot, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)

# 6.8) Eterogeneità del legame: random slope di KPI_B (se supportato)
form_slope <- update(form_base, . ~ . - (1|paziente) + (1 + KPI_B | paziente))
mod_slope <- try(lmer(form_slope, data = df, REML = TRUE, na.action = na.omit,
                      control = lmerControl(check.conv.singular = "ignore")), silent = TRUE)

if (!inherits(mod_slope, "try-error")) {
  print(summary(mod_slope))
  print(VarCorr(mod_slope))    # Varianza random slope KPI_B
  # Confronto (attenzione: REML -> per confronto meglio ML)
  mod_base_ml  <- update(mod_base, REML = FALSE)
  mod_slope_ml <- update(mod_slope, REML = FALSE)
  print(anova(mod_base_ml, mod_slope_ml))
} else {
  message("Modello con random slope non stimabile in modo stabile (singular fit).")
}

# 6.9) Report sintetico rispetto alle soglie operative

report <- list(
  MCID = MCID,
  overall_RMSE = overall_RMSE,
  overall_MAE  = overall_MAE,
  CV_metrics   = cv_metrics,
  calib_equivalence_intercept_pm0_2SD = equiv_interc,  # ±0.2 SD
  calib_equivalence_slope_0_9_to_1_1  = equiv_slope,   # 0.9–1.1
  BA_cond_bias = ba_bias,
  BA_cond_LoA  = c(lower = ba_LoA_l, upper = ba_LoA_u),
  Residual_corr    = corr_e,
  Residual_corr_CI = quantile(corr_boot, probs = c(0.025, 0.975), na.rm = TRUE)
)
report


# =========================
# FINE PIPELINE
# =========================
