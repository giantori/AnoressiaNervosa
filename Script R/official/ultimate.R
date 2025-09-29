library(dplyr)
library(readxl)
library(tidyr)
library(janitor)
library(DataExplorer)
library(writexl)
library(glue)
library(lattice)
library(ggplot2)
library(pheatmap)
library(gridExtra)  # per affiancare grafici e tabelle
library(knitr)
library(openxlsx)

today <- Sys.Date()
dati_lunghi_path <- glue("output/dati_lunghi_{today}_AN.xlsx")

# -- carichiamo i nostri dati

dati_larghi <- read_excel("output/dati_larghi_2025-07-02_AN.xlsx", n_max = 25)
dati_lunghi <- read_excel(dati_lunghi_path)

dati_lunghi_ff <- read_excel("dati_long_ff.xlsx")
dati_lunghi_mc <- read_excel("dati_long_mc.xlsx")


