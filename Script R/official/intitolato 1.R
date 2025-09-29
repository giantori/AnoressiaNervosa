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
x <- read_excel(path_X)
mc <- read_excel(path_A)
ff <- read_excel(path_B)

# Se preferisci avere i singoli grafici come lista:
# plots_list <- plot_box_by_group(df_all, y_var = "RMR", group = "suss", combine = FALSE)
# # E poi, ad esempio, stampi il primo:
# plots_list[["situptest"]]

df_all <- left_join(mc, ff, by = c('paziente', 'tempo'))

# gruppi fissi
.suss_vars     <- c("situptest", "squattest", "chairstandtest")
.handgrip_vars <- c("sx1","sx2","sx3","dx1","dx2","dx3","mediasx","mediadx","dssx","dsdx")

# df:    data frame con le colonne richieste
# y_var: stringa, nome della variabile da mettere in asse Y (es. "RMR")
# group: "suss" oppure "handgrip"
# combine: se TRUE ritorna un patchwork con tutti i grafici in griglia; se FALSE ritorna una lista di ggplot
# ncol:  quante colonne nella griglia (se combine = TRUE)
plot_box_by_group <- function(df, y_var, group = c("suss","handgrip"),
                              combine = TRUE, ncol = 3) {
  group <- match.arg(group)
  vars  <- if (group == "suss") .suss_vars else .handgrip_vars
  
  # crea i singoli plot: x = ogni variabile del gruppo, y = y_var
  plots <- lapply(vars, function(v) {
    ggplot(df, aes(x = factor(.data[[v]]), y = .data[[y_var]])) +
      geom_boxplot(na.rm = TRUE) +
      labs(
        x = v,
        y = y_var,
        title = paste("Boxplot di", y_var, "per", v)
      ) +
      theme_minimal()
  })
  names(plots) <- vars
  
  if (combine) {
    return(wrap_plots(plots, ncol = ncol))
  } else {
    return(plots)
  }
}

# ""       ""      "FFMI"     <-  questa è speciale nel senso si lega solo a 4 kpi di ff 



# RMR ---------------------------------------------------------------------


# Tutti i boxplot di RMR contro le variabili SUSS, combinati in una griglia 2x2
plot_box_by_group(df_all, y_var = "RMR", group = "suss", combine = TRUE, ncol = 2)

# Tutti i boxplot di RMR contro le variabili HANDGRIP, in griglia 3 colonne
plot_box_by_group(df_all, y_var = "RMR", group = "handgrip", combine = TRUE, ncol = 3)

# BCM ---------------------------------------------------------------------


# Tutti i boxplot di RMR contro le variabili SUSS, combinati in una griglia 2x2
plot_box_by_group(df_all, y_var = "BCM", group = "suss", combine = TRUE, ncol = 2)

# Tutti i boxplot di RMR contro le variabili HANDGRIP, in griglia 3 colonne
plot_box_by_group(df_all, y_var = "BCM", group = "handgrip", combine = TRUE, ncol = 3)


# pha ---------------------------------------------------------------------


# Tutti i boxplot di RMR contro le variabili SUSS, combinati in una griglia 2x2
plot_box_by_group(df_all, y_var = "pha", group = "suss", combine = TRUE, ncol = 2)

# Tutti i boxplot di RMR contro le variabili HANDGRIP, in griglia 3 colonne
plot_box_by_group(df_all, y_var = "pha", group = "handgrip", combine = TRUE, ncol = 3)

# BCMI ---------------------------------------------------------------------


# Tutti i boxplot di RMR contro le variabili SUSS, combinati in una griglia 2x2
plot_box_by_group(df_all, y_var = "BCMI", group = "suss", combine = TRUE, ncol = 2)

# Tutti i boxplot di RMR contro le variabili HANDGRIP, in griglia 3 colonne
plot_box_by_group(df_all, y_var = "BCMI", group = "handgrip", combine = TRUE, ncol = 3)


# rx  -----------------------------------------------------------------


# Tutti i boxplot di RMR contro le variabili SUSS, combinati in una griglia 2x2
plot_box_by_group(df_all, y_var = "rx", group = "suss", combine = TRUE, ncol = 2)

# Tutti i boxplot di RMR contro le variabili HANDGRIP, in griglia 3 colonne
plot_box_by_group(df_all, y_var = "rx", group = "handgrip", combine = TRUE, ncol = 3)


# xc ----------------------------------------------------------------------



# Tutti i boxplot di RMR contro le variabili SUSS, combinati in una griglia 2x2
plot_box_by_group(df_all, y_var = "xc", group = "suss", combine = TRUE, ncol = 2)

# Tutti i boxplot di RMR contro le variabili HANDGRIP, in griglia 3 colonne
plot_box_by_group(df_all, y_var = "xc", group = "handgrip", combine = TRUE, ncol = 3)


# FM ----------------------------------------------------------------------




# Tutti i boxplot di RMR contro le variabili SUSS, combinati in una griglia 2x2
plot_box_by_group(df_all, y_var = "FM", group = "suss", combine = TRUE, ncol = 2)

# Tutti i boxplot di RMR contro le variabili HANDGRIP, in griglia 3 colonne
plot_box_by_group(df_all, y_var = "FM", group = "handgrip", combine = TRUE, ncol = 3)


# FMp ---------------------------------------------------------------------


# Tutti i boxplot di RMR contro le variabili SUSS, combinati in una griglia 2x2
plot_box_by_group(df_all, y_var = "FMp", group = "suss", combine = TRUE, ncol = 2)

# Tutti i boxplot di RMR contro le variabili HANDGRIP, in griglia 3 colonne
plot_box_by_group(df_all, y_var = "FMp", group = "handgrip", combine = TRUE, ncol = 3)




# TBW ---------------------------------------------------------------------


# Tutti i boxplot di RMR contro le variabili SUSS, combinati in una griglia 2x2
plot_box_by_group(df_all, y_var = "TBW", group = "suss", combine = TRUE, ncol = 2)


# TBWp --------------------------------------------------------------------



# Tutti i boxplot di RMR contro le variabili SUSS, combinati in una griglia 2x2
plot_box_by_group(df_all, y_var = "TBWp", group = "suss", combine = TRUE, ncol = 2)



# ECW ---------------------------------------------------------------------



# Tutti i boxplot di RMR contro le variabili SUSS, combinati in una griglia 2x2
plot_box_by_group(df_all, y_var = "ECW", group = "suss", combine = TRUE, ncol = 2)


# ECWp --------------------------------------------------------------------



# Tutti i boxplot di RMR contro le variabili SUSS, combinati in una griglia 2x2
plot_box_by_group(df_all, y_var = "ECWp", group = "suss", combine = TRUE, ncol = 2)


# ICW ---------------------------------------------------------------------



# Tutti i boxplot di RMR contro le variabili SUSS, combinati in una griglia 2x2
plot_box_by_group(df_all, y_var = "ICW", group = "suss", combine = TRUE, ncol = 2)


# ICWp --------------------------------------------------------------------



# Tutti i boxplot di RMR contro le variabili SUSS, combinati in una griglia 2x2
plot_box_by_group(df_all, y_var = "ICWp", group = "suss", combine = TRUE, ncol = 2)

