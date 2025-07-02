dati <- read_excel("output/dati_larghi_2025-06-26_AN.xlsx", n_max = 25)

dati_long <- dati %>%
  pivot_longer(
    cols = -c("sintomo_1", "sintomo_2", "arto_dom", "altezza_m", 
              "sesso", "eta", "paziente"),
    names_to = c(".value", "tempo"),
    names_sep = "_"
  )

dati_long$tempo <- recode(dati_long$tempo, "T0" = 0, "T1" = 1, "T2" = 2)
View(dati_long)
dim(dati_long)

## lattice plot per sx1, sx2, sx3

# Inclusione della colonna che distingue le prove
dati_long_prove <- dati_long %>%
  pivot_longer(
    cols = c(sx1, sx2, sx3),
    names_to = "prova",
    values_to = "forza"
  )

# grafico lattice plot prova 3
xyplot(forza ~ tempo | factor(paziente),
       data = dati_long_prove,
       groups = prova,
       type = "b",  # "b" = punti + linee
       pch = 16,    # tipo di punto (16 = cerchio pieno)
       auto.key = list(title = "Prova", columns = 3),
       xlab = "Tempo",
       ylab = "Forza sx",
       main = "Andamento delle prove sx nel tempo per soggetto")

# Salvataggio dati_long output
today <- Sys.Date()
output_path <- glue("output/dati_lunghi_{today}_AN.xlsx")
write_xlsx(dati, path = output_path)

