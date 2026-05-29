test_that("rozmyty_topsis zwraca poprawna klase i dlugosc rankingu", {
  data("mcda_dane_surowe", package = "DreamySleepR")
  
  skladnia <- "
    Dokladnosc =~ dokladnosc_faz_snu;
    Analiza =~ analiza_statystyk;
    UX =~ ux_latwosc;
    Fizjologia =~ dane_fizjologiczne;
    Integracja =~ integracja_urzadzenia;
    Funkcje =~ funkcje_dodatkowe;
    Bateria =~ zuzycie_baterii;
    Cena =~ cena_subskrypcja
  "
  
  dane_rozmyte <- przygotuj_dane_mcda(
    dane = mcda_dane_surowe,
    skladnia = skladnia,
    kolumna_alternatyw = "Aplikacja"
  )
  
  macierz <- as.matrix(dane_rozmyte)
  typy <- c("max", "max", "max", "max", "max", "max", "min", "min")
  
  # Przykladowe wagi
  wagi <- rep(1/8, 8)
  
  wynik <- rozmyty_topsis(
    macierz_decyzyjna = macierz,
    typy_kryteriow = typy,
    wagi = wagi
  )
  
  # Sprawdzenie klasy
  expect_s3_class(wynik, "rozmyty_topsis_wynik")
  
  # Sprawdzenie czy wynik zawiera dane dla wszystkich 3 alternatyw
  expect_equal(nrow(wynik$wyniki), 3)
  
  # Sprawdzenie rang
  expect_true("Ranking" %in% colnames(wynik$wyniki))
  expect_equal(sort(wynik$wyniki$Ranking), 1:3)
})
