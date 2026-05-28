test_that("rozmyty_meta_ranking zwraca poprawna klase i kolumny", {
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
  
  kryteria <- c("Dokladnosc", "Analiza", "UX", "Fizjologia", "Integracja", "Funkcje", "Bateria", "Cena")
  najlepsze_do_innych <- c(1, 2, 3, 2, 2, 2, 4, 5)
  inne_do_najgorszego <- c(5, 4, 3, 4, 4, 4, 2, 1)
  
  wynik <- rozmyty_meta_ranking(
    macierz_decyzyjna = macierz,
    typy_kryteriow = typy,
    bwm_kryteria = kryteria,
    bwm_najlepsze = najlepsze_do_innych,
    bwm_najgorsze = inne_do_najgorszego
  )
  
  # Sprawdzenie klasy
  expect_s3_class(wynik, "rozmyty_meta_ranking_wynik")
  
  # Sprawdzenie kolumn w tabeli porownawczej
  oczekiwane_kolumny <- c("Alternatywa", "R_TOPSIS", "R_VIKOR", "R_MULTIMOORA", "Meta_Suma", "Meta_Dominacja", "Meta_Agregacja")
  expect_true(all(oczekiwane_kolumny %in% colnames(wynik$porownanie)))
  
  # Sprawdzenie liczby alternatyw (3)
  expect_equal(nrow(wynik$porownanie), 3)
})
