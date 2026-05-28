set.seed(2026)

aplikacje <- c("AutoSleep", "Sleep as Android", "Garmin Sleep Advanced")

profil_jakosci <- data.frame(
  dokladnosc_faz_snu = c(7, 8, 5),
  analiza_statystyk = c(6, 7, 4),
  ux_latwosc = c(8, 5, 5),
  dane_fizjologiczne = c(5, 6, 4),
  integracja_urzadzenia = c(5, 4, 8),
  funkcje_dodatkowe = c(7, 6, 5),
  zuzycie_baterii = c(5, 4, 8),
  cena_subskrypcja = c(4, 5, 7),
  row.names = aplikacje
)

skala <- 1.5

dane <- expand.grid(
  EkspertID = 1:15,
  Aplikacja = aplikacje
)

n <- nrow(dane)

kryteria <- names(profil_jakosci)

for (kryt in kryteria) {
  wartosci <- numeric(n)
  for (i in seq_len(n)) {
    apka <- as.character(dane$Aplikacja[i])
    srodek <- profil_jakosci[apka, kryt]
    surowa <- abs(rcauchy(1, location = srodek, scale = skala))
    wartosci[i] <- round(min(max(surowa, 1), 9))
  }
  dane[[kryt]] <- wartosci
}

mcda_dane_surowe <- dane

usethis::use_data(
  mcda_dane_surowe,
  overwrite = TRUE
)
