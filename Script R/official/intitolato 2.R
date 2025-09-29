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



# =========================
x <- read_excel(path_X)

library(ggplot2)
library(dplyr)
library(patchwork)

# variabili originali
.suss_vars     <- c("situptest", "squattest", "chairstandtest")
.handgrip_sx   <- c("sx1","sx2","sx3","mediasx","dssx")
.handgrip_dx   <- c("dx1","dx2","dx3","mediadx","dsdx")

# mappa "coppie" sx/dx -> nome unificato dominante
.handgrip_map <- list(
  grip1 = c("sx" = "sx1",    "dx" = "dx1"),
  grip2 = c("sx" = "sx2",    "dx" = "dx2"),
  grip3 = c("sx" = "sx3",    "dx" = "dx3"),
  media = c("sx" = "mediasx","dx" = "mediadx"),
  ds    = c("sx" = "dssx",   "dx" = "dsdx")
)

# helper: crea colonne "dominanti" in base ad arto_dom
make_handgrip_dominant <- function(df, arto_col = "arto_dom") {
  if (!arto_col %in% names(df)) {
    stop("Colonna '", arto_col, "' non trovata nel data frame.")
  }
  ad <- tolower(as.character(df[[arto_col]]))
  if (!all(is.na(ad) | ad %in% c("sx","dx"))) {
    warning("Valori di arto_dom diversi da 'sx'/'dx' saranno messi a NA.")
  }
  # crea le colonne unificate
  for (new_name in names(.handgrip_map)) {
    sx_col <- .handgrip_map[[new_name]][["sx"]]
    dx_col <- .handgrip_map[[new_name]][["dx"]]
    # controlli di esistenza
    if (!all(c(sx_col, dx_col) %in% names(df))) {
      stop("Mancano le colonne '", sx_col, "' o '", dx_col, "' nel data frame.")
    }
    df[[new_name]] <- ifelse(
      ad == "sx", df[[sx_col]],
      ifelse(ad == "dx", df[[dx_col]], NA_real_)
    )
  }
  df
}

# df: data frame
# y_var: nome variabile Y (stringa), es. "RMR"
# group: "suss" oppure "handgrip"
# combine: TRUE per una griglia unica; FALSE per lista di ggplot
# ncol: colonne nella griglia se combine=TRUE
plot_box_by_group <- function(df, y_var, group = c("suss","handgrip"),
                              combine = TRUE, ncol = 3, arto_col = "arto_dom") {
  group <- match.arg(group)
  
  if (!y_var %in% names(df)) stop("La variabile Y '", y_var, "' non esiste nel data frame.")
  
  if (group == "suss") {
    vars <- .suss_vars
    if (!all(vars %in% names(df))) stop("Mancano una o più variabili SUSS nel data frame.")
    plots <- lapply(vars, function(v) {
      ggplot(df, aes(x = factor(.data[[v]]), y = .data[[y_var]])) +
        geom_boxplot(na.rm = TRUE) +
        labs(x = v, y = y_var, title = paste("Boxplot di", y_var, "per", v)) +
        theme_minimal()
    })
    names(plots) <- vars
  } else {
    # HANDGRIP: crea colonne dominanti e usa solo quelle
    df_dom <- make_handgrip_dominant(df, arto_col = arto_col)
    dom_vars <- names(.handgrip_map)  # c("grip1","grip2","grip3","media","ds")
    plots <- lapply(dom_vars, function(v) {
      ggplot(df_dom, aes(x = factor(.data[[v]]), y = .data[[y_var]])) +
        geom_boxplot(na.rm = TRUE) +
        labs(x = paste0(v, " (dominante)"), y = y_var,
             title = paste("Boxplot di", y_var, "per", v, "(dominante)")) +
        theme_minimal()
    })
    names(plots) <- dom_vars
  }
  
  if (combine) {
    return(wrap_plots(plots, ncol = ncol))
  } else {
    return(plots)
  }
}

# Y = RMR, gruppo SUSS (usa le 3 colonne originali)
plot_box_by_group(df_all, y_var = "RMR", group = "suss", combine = TRUE, ncol = 2)

# Y = RMR, gruppo HANDGRIP (usa SOLO le colonne del lato dominante per ciascuna riga)
plot_box_by_group(x, y_var = "RMR", group = "handgrip", combine = TRUE, ncol = 3)



