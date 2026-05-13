set.seed(123)

aplikacje <- c(
  "AutoSleep",
  "Sleep_as_Android",
  "Garmin_Sleep_Advanced"
)

dane <- expand.grid(
  EkspertID = 1:6,
  Aplikacja = aplikacje
)

n <- nrow(dane)

dane$dokladnosc_faz_snu <- sample(1:5, n, replace = TRUE)

dane$analiza_statystyk <- sample(1:5, n, replace = TRUE)

dane$ux_latwosc <- sample(1:5, n, replace = TRUE)

dane$dane_fizjologiczne <- sample(1:5, n, replace = TRUE)

dane$integracja_urzadzenia <- sample(1:5, n, replace = TRUE)

dane$funkcje_dodatkowe <- sample(1:5, n, replace = TRUE)

dane$zuzycie_baterii <- sample(1:5, n, replace = TRUE)

dane$cena_subskrypcja <- sample(1:5, n, replace = TRUE)

mcda_dane_surowe <- dane

usethis::use_data(
  mcda_dane_surowe,
  overwrite = TRUE
)