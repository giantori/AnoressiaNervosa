library(dplyr)
library(readxl)
library(tidyr)
library(janitor)
library(DataExplorer)
library(writexl)
library(glue)
library(lattice)
library(ggplot2)

dati <- read_excel("output/dati_larghi_2025-07-02_AN.xlsx", n_max = 25)

dati_long <- dati %>%
  pivot_longer(
    cols = -c("sintomo_1", "sintomo_2", "arto_dom", "altezza_m", 
              "sesso", "eta", "paziente"),
    names_to = c(".value", "tempo"),
    names_sep = "_"
  )

dati_long$tempo <- recode(dati_long$tempo, "T0" = 0, "T1" = 1, "T2" = 2)

## lattice plot per sx1, sx2, sx3

# Creazione del dataset lungo distinguendo per ogni soggetto le tre prove sx 
# e le tre prove dx
dati_long_provesx <- dati_long %>%
  pivot_longer(
    cols = c(sx1, sx2, sx3),
    names_to = "provasx",
    values_to = "forzasx"
  )

dati_long_provedx <- dati_long %>%
  pivot_longer(
    cols = c(dx1, dx2, dx3),
    names_to = "provadx",
    values_to = "forzadx"
  )

dati_long_prove <- bind_cols(
  dati_long_provesx,
  dati_long_provedx %>% select(provadx, forzadx)  # Evita di duplicare le altre colonne
)


# grafico lattice plot prove sx 
xyplot(forzasx ~ tempo | factor(paziente),
       data = dati_long_prove,
       groups = provasx,
       type = "b",  # "b" = punti + linee
       pch = 16,    # tipo di punto (16 = cerchio pieno)
       auto.key = list(title = "Prova", columns = 3),
       xlab = "Tempo",
       ylab = "Forza sx",
       main = "Andamento delle prove sx nel tempo per soggetto")

# grafico lattice plot prove dx 
xyplot(forzadx ~ tempo | factor(paziente),
       data = dati_long_prove,
       groups = provadx,
       type = "b",  # "b" = punti + linee
       pch = 16,    # tipo di punto (16 = cerchio pieno)
       auto.key = list(title = "Prova", columns = 3),
       xlab = "Tempo",
       ylab = "Forza dx",
       main = "Andamento delle prove dx nel tempo per soggetto")

# grafico lattice plot per sx e dx 
dati_long_plot <- dati_long_prove %>%
  select(paziente, tempo, provasx, forzasx, provadx, forzadx) %>%
  pivot_longer(
    cols = c(forzasx, forzadx),
    names_to = "lato",
    names_prefix = "forza",
    values_to = "forza"
  ) %>%
  mutate(
    prova = ifelse(lato == "sx", provasx, provadx),
    paz_lato = paste0(paziente, "_", lato)  # soggetto + lato
  )

xyplot(forza ~ tempo | paz_lato,
       data = dati_long_plot,
       groups = prova,
       type = "b",
       pch = 16,
       auto.key = list(title = "Prove per lato", columns = 3),
       xlab = "Tempo",
       ylab = "Forza",
       main = "Prove di forza lato sx e lato dx nel tempo per soggetto",
       layout = c(6, 7))  # es: 6 colonne, 7 righe per 42 pannelli

# arto con maggiore forza per ciascun paziente 
forza_media_lato <- dati_long_plot %>%
  group_by(paziente, lato) %>%
  summarise(forza_media = mean(forza, na.rm = TRUE), .groups = "drop")


# Confronto: lato più forte per ogni paziente
lato_piu_forte <- forza_media_lato %>%
  group_by(paziente) %>%
  slice_max(forza_media) %>%
  select(paziente, lato, forza_media)

ggplot(forza_media_lato, aes(x = paziente, y = forza_media, fill = lato)) +
  geom_col(position = "dodge") +
  labs(title = "Forza media per lato e soggetto", y = "Forza media") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# grafico lattice plot per la forza fisica 


# Salvataggio dati_long output
today <- Sys.Date()
output_path <- glue("output/dati_lunghi_{today}_AN.xlsx")
write_xlsx(dati, path = output_path)

