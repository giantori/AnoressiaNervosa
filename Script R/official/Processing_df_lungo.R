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

# grafico lattice plot per le prove situptest, Squat Test, Chair stand Test, 
# Sit reach Test

xyplot(situptest ~ tempo | factor(paziente),
       data = dati_long,
       type = "b",       # punti + linee
       pch = 16,         # cerchio pieno
       xlab = "Tempo",
       ylab = "Sit-up Test",
       main = "Andamento del Sit-up Test nel tempo per soggetto")

xyplot(squattest ~ tempo | factor(paziente),
       data = dati_long,
       type = "b",       # punti + linee
       pch = 16,         # cerchio pieno
       xlab = "Tempo",
       ylab = "Squat Test",
       main = "Andamento del Sit-up Test nel tempo per soggetto")

xyplot(chairstandtest ~ tempo | factor(paziente),
       data = dati_long,
       type = "b",       # punti + linee
       pch = 16,         # cerchio pieno
       xlab = "Tempo",
       ylab = "Chair stand Test",
       main = "Andamento del Sit-up Test nel tempo per soggetto")

xyplot(sitreachtest ~ tempo | factor(paziente),
       data = dati_long,
       type = "b",       # punti + linee
       pch = 16,         # cerchio pieno
       xlab = "Tempo",
       ylab = "Sit reach Test",
       main = "Andamento del Sit-up Test nel tempo per soggetto")


# correlazione tra variabili (marcatori biologici)

# Approfondimento teorico di sitreachtest, chairstandtest, squattest, situptest

# BOXPLOT per marcatori clinici 
# Asse delle y: 
# valori assunti dal marcatore clinico 
# Asse delle x:
# Creare una variabile categoriale partendo da mediasx (fare lo stesso 
# per mediadx) e distinguere 3 categorie, es basso medio alto e 
# quindi costruire tanti boxplot per il marcatore A tanti quante sono 
# le categorie della nuova variabile mediasx.

# Questo svilupparlo per ogni marcatore clinico.
# Evidenziare le differenze che ci sono per lato destro e lato sinistro 
# Osservare così se a valori bassi o alti di forza sono associati valori
# bassi o alti del marcatore clinico A.

# Con quelli in cui la relazione è maggiormente evidente 
# proviamo anche a procedere con la stessa metodologia distinguendo
# non per forza ma per valori dei test di funzionalità motoria.

# Sottolineare però che queste relazioni bivariate non tengono conto 
# del resto delle variabili e non ci permettono di fare inferenza ma 
# solo di descrivere una relazione senza tener conto di tutto il resto
# AGGIUSTARE

# Correzione valori sitreachest
unique(dati_long$sitreachtest)

dati_long$sitreachtest[dati_long$sitreachtest < 0] %>% length()
dati_long$sitreachtest = gsub("-13", "13", dati_long$sitreachtest)
dati_long$sitreachtest = gsub("-8", "8", dati_long$sitreachtest)

dati_long$sitreachtest = as.numeric(dati_long$sitreachtest)


## Boxplot relazione marker clinici con variabili riferite alla forza fisica 

# Trasformazione della variabile chairstandtest in una variabile categorica
dati_long$chairstandtest = as.integer(dati_long$chairstandtest)  # deve essere numerica
intervalli <- c(0,9,15,20)  # esempio
etichette <- c("0-9", "10-15", "16-20")  # esempio

unique(dati_long$chairstandtest)

dati_long <- dati_long %>%
  mutate(
    chairstandtest_ = cut(
      chairstandtest,
      breaks = c(0, 9, 15, 20),
      labels = c("0-9", "10-15", "16-20"),
      include.lowest = TRUE
    )
  )

# Trasformazione della variabile sitreachtest in una variabile categorica
dati_long$sitreachtest = as.integer(dati_long$sitreachtest)  # deve essere numerica
intervalli <- c(0, 5, 16, 25)  # esempio
etichette <- c("0-5", "6-16", "17-25") # esempio

# Trasformazione della variabile in una variabile categorica
dati_long <- dati_long %>%
  mutate(
    sitreachtest_ = cut(
      sitreachtest,
      breaks = c(0, 5, 16, 25),
      labels = c("0-5", "6-16", "17-25"),
      include.lowest = TRUE
    )
  )

# 2) Metti in formato long le 4 variabili categoriali
vars_cat <- c("situptest", "squattest","chairstandtest_","sitreachtest_")
str(dati_long)

# Verifica che ci siano tutte
stopifnot(all(vars_cat %in% names(dati_long)))
stopifnot("pha" %in% names(dati_long))

d_long <- dati_long %>%
  mutate(across(all_of(vars_cat), as.factor)) %>%
  pivot_longer(
    cols = all_of(vars_cat),
    names_to = "test",
    values_to = "categoria"
  )


p <- ggplot(d_long, aes(x = categoria, y = pha, fill = categoria)) +
  geom_boxplot(outlier.alpha = 0.6) +
  facet_wrap(~ test, scales = "free_x", ncol = 2) +
  labs(
    x = "Test di performance fisica",
    y = "Phase Angle",
    title = "Boxplot di PHA per categoria dei test",
    subtitle = "Pannelli: situptest, squattest, chairstandtest, sitreachtest"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.spacing = unit(1, "lines"),
    legend.position = "none"   # puoi togliere o tenere la legenda
  )

print(p)

# Farlo per tutte le variabili 

summary(dati_long$pha)

-# Analisi delle componenti principali -----

# Suddivisione del dataset:
# - dati_long_mc : contiene solo i marker clinici
# - dati_long_ff: contiene solo le variabili delle prove di forza fisica

## Marcatori biologici ----

### Set completo di variabili mc ----

# dati riferiti alle variabili mc 
dati_long_mc <- dati_long %>%
  select(paziente, tempo, RMR, VO2, RQ, rx, xc, FM, FMp, FFMI, TBW, TBWp, ECW, ECWp, ICW, ICWp, 
         BCM, pha, BCMI)

# Salvataggio dati_long_mc
write.xlsx(dati_long_mc, file = "dati_long_mc.xlsx")

# dati riferiti alle variabili mc 
dati_long_mc <- dati_long_mc %>%
  select(RMR, VO2, RQ, rx, xc, FM, FMp, FFMI, TBW, TBWp, ECW, ECWp, ICW, ICWp, 
         BCM, pha, BCMI)

# analisi delle componenti principali per dati_long_mc 
dati_long_mc_pca <- princomp(dati_long_mc, cor = TRUE, scores = TRUE)
loadings_mc <- dati_long_mc_pca$loadings  # matrice variabili × componenti
loadings_mc_mat <- as.matrix(loadings_mc)

####heatmap mc----
# heatmap dei loadings per le prime 6 componenti principali 
pheatmap(loadings_mc_mat[, 1:6],
         cluster_rows = TRUE,
         cluster_cols = FALSE, 
         main = "Heatmap dei loadings PCA (componenti 1–6)")

#### screegraph mc ----
summary(dati_long_mc_pca)
plot(dati_long_mc_pca)


#### contributo alla varianza spiegata totale mc ----
eigenvalues_mc <- dati_long_mc_pca$sdev[1:3]^2
loadings_mc[,1:3]
# Calcolo dell'indice di contributo per variabile
ind_mc <- apply(loadings_mc[,1:3]^2, 1, function(x) sum(x / eigenvalues_mc))

#### tabella e grafico mc ----
tabella_mc <- data.frame(Variabile = names(ind_mc),
                         Contributo = ind_mc)
tabella_mc <- tabella_mc[order(-tabella_mc$Contributo), ]  # ordina decrescente

library(ggplot2)

ggplot(tabella_mc, aes(x = reorder(Variabile, Contributo), y = Contributo, fill = Contributo)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_gradient(low = "#CDE1F2", high = "#1B4F72") +
  labs(
    title = "Contributo delle variabili alla varianza totale (PCA)",
    x = "Variabili",
    y = "Indice di Contributo"
  ) +
  theme_minimal(base_size = 13)

### Riduzione del set di variabili mc----

# Proviamo a costruire le pca considerando le variabili più importanti
dati_long_mc_ridotto = dati_long_mc %>% select(-"RQ", -"BCM", -"FFMI", 
                                                       -"BCMI", -"RMR", -"VO2")
dati_long_mc_pca_ridotto <- princomp(dati_long_mc_ridotto, cor = TRUE, scores = TRUE)
loadings_mc_ridotto <- dati_long_mc_pca_ridotto$loadings 
loadings_mc_mat_ridotto <- as.matrix(loadings_mc_ridotto)
pheatmap(loadings_mc_mat_ridotto[, 1:6],
         cluster_rows = TRUE,
         cluster_cols = FALSE, 
         main = "Heatmap dei loadings PCA (componenti 1–6)")

#### screegraph mc ridotto -----
summary(dati_long_mc_pca_ridotto)
plot(dati_long_mc_pca_ridotto)

#### contributo alla varianza spiegata totale mc ridotto ----
eigenvalues_mc_ridotto <- dati_long_mc_pca_ridotto$sdev[1:3]^2
loadings_mc_ridotto[,1:3]
# Calcolo dell'indice di contributo per variabile
ind_mc_ridotto <- apply(loadings_mc_ridotto[,1:3]^2, 1, function(x) sum(x / eigenvalues_mc_ridotto))

#### tabella e grafico mc ridotto ----
tabella_mc_ridotto <- data.frame(Variabile = names(ind_mc_ridotto),
                         Contributo = ind_mc_ridotto)
tabella_mc_ridotto <- tabella_mc_ridotto[order(-tabella_mc_ridotto$Contributo), ]  # ordina decrescente

ggplot(tabella_mc_ridotto, aes(x = reorder(Variabile, Contributo), y = Contributo, fill = Contributo)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_gradient(low = "#CDE1F2", high = "#1B4F72") +
  labs(
    title = "Contributo delle variabili alla varianza totale (PCA)",
    x = "Variabili",
    y = "Indice di Contributo"
  ) +
  theme_minimal(base_size = 13)

# Salvataggio dati_long_mc_ridotto
write.xlsx(dati_long_mc_ridotto, file = "dati_long_mc_ridotto.xlsx")


## Forza fisica ----

# dati riferiti alle variabili ff 
dati_long_ff <- dati_long %>%
  select(paziente, tempo, sx1, sx2, sx3, dx1, dx2, dx3, mediasx, mediadx, dssx, dsdx, situptest,
         squattest, chairstandtest, sitreachtest)

# Salvataggio dati_long_ff
write.xlsx(dati_long_ff, file = "dati_long_ff.xlsx")

# dati riferiti alle variabili ff 
dati_long_ff <- dati_long_ff %>%
  select(sx1, sx2, sx3, dx1, dx2, dx3, mediasx, mediadx, dssx, dsdx, situptest,
         squattest, chairstandtest, sitreachtest)

# analisi delle componenti principali per dati_long_ff 
dati_long_ff_pca <- princomp(dati_long_ff, cor = TRUE, scores = TRUE)
loadings_ff <- dati_long_ff_pca$loadings  # matrice variabili × componenti
loadings_ff_mat <- as.matrix(loadings_ff)
 
### pheatmap ff ----
#heatmap dei loadings per le prime 6 componenti principali 
pheatmap(loadings_ff_mat[, 1:6],
         cluster_rows = TRUE,
         cluster_cols = FALSE, 
         main = "Heatmap dei loadings delle variabili ff PCA (componenti 1–6)")

### screegraph ff ----
summary(dati_long_ff_pca)
plot(dati_long_ff_pca)

### contributo alla varianza spiegata totale ff --
eigenvalues_ff <- dati_long_ff_pca$sdev[1:3]^2
loadings_mc[,1:3]
# Calcolo dell'indice di contributo per variabile
ind_mc <- apply(loadings_mc[,1:3]^2, 1, function(x) sum(x / eigenvalues_mc))

### tabella e grafico ff ----
tabella_ff <- data.frame(Variabile = names(ind_mc),
                         Contributo = ind_mc)
tabella_mc <- tabella_mc[order(-tabella_mc$Contributo), ]  # ordina decrescente

library(ggplot2)

ggplot(tabella_mc, aes(x = reorder(Variabile, Contributo), y = Contributo, fill = Contributo)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_gradient(low = "#CDE1F2", high = "#1B4F72") +
  labs(
    title = "Contributo delle variabili alla varianza totale (PCA)",
    x = "Variabili",
    y = "Indice di Contributo"
  ) +
  theme_minimal(base_size = 13)


## punteggi (scores) di PCA su mc e ff -----
pc1_ff = dati_long_ff_pca$scores[,1]
pc1_mc = dati_long_mc_pca_ridotto$scores[,1]

## plot tra pc1_mc e pc1_ff -----
plot(x = pc1_mc, y = pc1_ff)

summary(dati_long)
table(dati_long$situptest)


# Salvataggio dati_long output
today <- Sys.Date()
output_path <- glue("output/dati_lunghi_{today}_AN.xlsx")
write_xlsx(dati_long, path = output_path)

